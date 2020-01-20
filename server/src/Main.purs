module Main where

import Prelude
import Data.Array (fromFoldable, head, (..))
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.Posix.Signal (Signal(..))
import Data.String.Utils (lines)
import Data.Time.Duration (Seconds(..), fromDuration)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, Milliseconds(..), bracket, delay, effectCanceler, launchAff_, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info, log)
import Effect.Exception (throw)
import Foreign (unsafeToForeign)
import JobQueue (EnqueueResult(..), NewJob(..), Queue, ResourcePool(..))
import JobQueue as Q
import Middleware.JsonBodyParser (jsonBodyParser)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess (ExecResult, defaultExecOptions, exec, kill, pid)
import Node.Encoding (Encoding(..))
import Node.Express.App (App, listenHttp, listenHttps, use, useExternal)
import Node.Express.App as E
import Node.Express.Handler (HandlerM(..), Handler)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody')
import Node.Express.Response (setStatus)
import Node.Express.Response as Response
import Node.Express.Types (Request, Response)
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.HTTP (Server)
import Node.OS (numCpus)
import Node.Process (lookupEnv)
import Shared.Json (readAff)
import Shared.Models.Body (RunResult, PursResult)
import Shared.Models.Body as Body
import Simple.JSON (read_, write)
import Simple.JSON as JSON

toBody ∷ ∀ r m. MonadEffect m => { stdout ∷ Buffer, stderr ∷ Buffer | r } -> m RunResult
toBody result =
  liftEffect
    $ ado
        stdout <- Buffer.toString UTF8 result.stdout
        stderr <- Buffer.toString UTF8 result.stderr
        let
          (code ∷ Maybe Int) = asErrorWithCode result >>= _.code
        in { code, stdout, stderr } ∷ RunResult

type ErrorWithCode
  = { code ∷ Maybe Int }

asErrorWithCode ∷ ∀ a. a -> Maybe ErrorWithCode
asErrorWithCode = read_ <<< unsafeToForeign

compileCode ∷ Folder -> String -> Aff PursResult
compileCode folder code = do
  saveMainFile folder code
  { stderr } <- execCommand folder "spago build --purs-args \"--json-errors\""
  strResult <- liftEffect $ Buffer.toString UTF8 stderr
  case checkOutput strResult of
    Just r -> pure r
    Nothing -> liftEffect $ throw $ "No result in " <> strResult
  where
  checkOutput output =
    head do
      line <- lines output
      let
        (parseable :: Maybe PursResult) = JSON.readJSON_ line
      fromFoldable parseable

runCode ∷ Folder -> Aff ExecResult
runCode folder = execCommand folder "node run.js"

saveMainFile ∷ Folder -> String -> Aff Unit
saveMainFile folder code = writeTextFile UTF8 (un Folder folder <> "/src/Main.purs") code

execCommand ∷ Folder -> String -> Aff ExecResult
execCommand folder command =
  makeAff \callback -> do
    let
      options = defaultExecOptions { cwd = Just (un Folder folder) }
    childProcess <- exec command options (callback <<< Right)
    pure $ effectCanceler ((log $ "Killing " <> show (pid childProcess)) *> kill SIGKILL childProcess)

newtype Folder
  = Folder String

derive instance ntFolder :: Newtype Folder _
derive newtype instance eqFolder :: Eq Folder
compileHandler ∷ Queue Folder -> HandlerM Unit
compileHandler queue = do
  body <- getBody'
  json <- readAff body # liftAff
  resultVar <- AVar.empty # liftEffect
  let
    cleanup ∷ Aff Unit
    cleanup = AVar.tryPut Nothing resultVar # liftEffect # void

    compileJob =
      NewJob \folder ->
        bracket (pure unit) (const cleanup) \_ -> do
          result <- compileCode folder (json ∷ Body.CompileRequest).code
          AVar.tryPut (Just result) resultVar # liftEffect # void
  queueRes <- Q.enqueue compileJob queue # liftEffect
  let
    getResult ∷ Handler
    getResult = do
      maybeResult <- AVar.tryRead resultVar # liftEffect
      case maybeResult of
        Just (Just result) -> Response.send $ write ({ result } ∷ Body.CompileResult)
        Just Nothing -> do
          setStatus 500
          Response.send $ write { error: "Timeout" }
        Nothing -> do
          delay (100.0 # Milliseconds) # liftAff
          getResult
  case queueRes of
    Enqueued _ -> getResult
    QueueFull -> do
      setStatus 500
      Response.send $ write { error: "Queue full" }

unHandler ∷ ∀ t6. Request -> Response -> Effect Unit -> HandlerM t6 -> Aff t6
unHandler req res next (HandlerM h) = h req res next

runHandler ∷ Queue Folder -> HandlerM Unit
runHandler queue =
  HandlerM \req res next -> do
    let
      handle = unHandler req res next

      runJob =
        NewJob \folder ->
          bracket (pure unit) (const (pure unit)) \_ -> do
            result <- runCode folder
            resultBody <- toBody result
            handle $ Response.send $ write (resultBody ∷ Body.RunResult)
    queueRes <- Q.enqueue runJob queue # liftEffect
    case queueRes of
      Enqueued _ -> pure unit
      QueueFull ->
        handle do
          setStatus 500
          Response.send $ write { error: "Queue full" }

main ∷ Effect Unit
main =
  launchAff_ do
    cpus <- numCpus # liftEffect
    let
      foldersToUse = min 2 (max 15 (cpus - 1))

      mkFolder = Folder <<< ("../playground" <> _) <<< show

      folderPool = ResourcePool $ mkFolder <$> (0 .. foldersToUse)
    q <-
      Q.mkQueue
        { maxSize: 50
        , timeout: 60.0 # Seconds # fromDuration
        }
        folderPool
    serverSetup (makeApp q)

makeApp ∷ Queue Folder -> App
makeApp q = do
  use $ static "assets"
  useExternal jsonBodyParser
  E.post "/compile" (compileHandler q)
  E.post "/run" (runHandler q)

data CertificatesToUse
  = UseLocalCertificates
  | DoNotUseLocalCertificates

parseCertificatesToUse ∷ String -> CertificatesToUse
parseCertificatesToUse = case _ of
  "LOCAL" -> UseLocalCertificates
  _ -> DoNotUseLocalCertificates

serverSetup ∷ App -> Aff Server
serverSetup app = do
  maybePortString <- lookupEnv "PORT" # liftEffect
  let
    port = maybePortString >>= fromString # fromMaybe 14188
  useLocalCertificates <- fromMaybe DoNotUseLocalCertificates <<< map parseCertificatesToUse <$> lookupEnv "CERTS" # liftEffect
  listen <- case useLocalCertificates of
    UseLocalCertificates -> do
      httpsOptions <- makeHttpsOptions
      pure $ listenHttps app (port ∷ Int) httpsOptions
    DoNotUseLocalCertificates -> pure $ listenHttp app port
  liftEffect $ listen \_ -> info $ "psfp server started on port " <> show port
  where
  makeHttpsOptions = do
    key <- readTextFile UTF8 "server.key"
    cert <- readTextFile UTF8 "server.cert"
    pure { key, cert }
