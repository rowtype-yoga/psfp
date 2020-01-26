module Main where

import Prelude

import Data.Array ((..))
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Posix.Signal (Signal(..))
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, launchAff_, makeAff, parallel, sequential)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info, log)
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
import Node.FS.Aff (readTextFile)
import Node.HTTP (Server)
import Node.OS (numCpus)
import Node.Process (lookupEnv)
import Playground.Playground (Folder(..), copy)
import PscIdeClient (PscIdeConnection, compileCode, getFolder, mkConnection)
import Shared.Json (readAff)
import Shared.Models.Body (CompileRequest, RunResult)
import Shared.Models.Body as Body
import Simple.JSON (read_, write)

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

-- compileCode ∷ JobId -> Folder -> Int -> String -> Aff PursResult
-- compileCode jobId folder port code = do
--   saveMainFile folder code
--   { stderr } <- execCommand folder "node_modules/spago/spago build --purs-args \"--json-errors\""
--   strResult <- liftEffect $ Buffer.toString UTF8 stderr
  
--   case checkOutput strResult of
--     Just r -> do
--       pure r
--     Nothing -> do
--       liftEffect $ throw $ "No result in " <> strResult
--   where
--   checkOutput output =
--     head do
--       line <- lines output
--       let
--         (parseable :: Maybe PursResult) = JSON.readJSON_ line
--       fromFoldable parseable

-- compileCodeOld ∷ JobId -> Folder -> String -> Aff PursResult
-- compileCodeOld jobId folder code = do
--   saveMainFile folder code
--   { stderr } <- execCommand folder "node_modules/spago/spago build --purs-args \"--json-errors\""
--   strResult <- liftEffect $ Buffer.toString UTF8 stderr
--   case checkOutput strResult of
--     Just r -> do
--       pure r
--     Nothing -> do
--       liftEffect $ throw $ "No result in " <> strResult
--   where
--   checkOutput output =
--     head do
--       line <- lines output
--       let
--         (parseable :: Maybe PursResult) = JSON.readJSON_ line
--       fromFoldable parseable

runCode ∷ Folder -> Aff ExecResult
runCode folder = execCommand folder "node run.js"

execCommand ∷ Folder -> String -> Aff ExecResult
execCommand folder command =
  makeAff \callback -> do
    let
      options = defaultExecOptions { cwd = Just (un Folder folder) }
    childProcess <- exec command options (callback <<< Right)
    pure $ effectCanceler ((log $ "Killing " <> show (pid childProcess)) *> kill SIGKILL childProcess)

compileAndRunJob ∷ CompileRequest -> (Handler -> Aff Unit) -> NewJob PscIdeConnection
compileAndRunJob json handle =
  NewJob \jobId conn -> do
    compileResult <- compileCode json.code conn 
    if compileResult.resultType == "error" then do
      handle $ setStatus 422
      handle $ Response.send $ write compileResult
    else do
      runResult <- runCode (getFolder conn)
      resultBody <- toBody runResult
      handle $ Response.send $ write (resultBody ∷ Body.RunResult)

compileAndRunHandler ∷ Queue PscIdeConnection -> Handler
compileAndRunHandler queue = do
  body <- getBody'
  json <- readAff body # liftAff
  HandlerM \req res next -> do
    let
      handle = unHandler req res next

      runJob = compileAndRunJob json handle
    queueRes <- Q.enqueue runJob queue # liftEffect
    case queueRes of
      Enqueued _ -> pure unit
      QueueFull ->
        handle do
          setStatus 500
          Response.send $ write { error: "Queue full" }

unHandler ∷ ∀ a. Request -> Response -> Effect Unit -> HandlerM a -> Aff a
unHandler req res next (HandlerM h) = h req res next

srcFolder ∷ String
srcFolder = "../playground"

destFolder ∷ String
destFolder = "../playgrounds/"

main ∷ Effect Unit
main = do
  launchAff_ do
    cpus <- numCpus # liftEffect
    let
      poolSize = max 2 (cpus - 1) -- use at least 2

      mkFolder = Folder <<< (destFolder <> _) <<< show

    connections <- sequential $ for (0 .. poolSize) \n -> parallel do
      let folder = mkFolder n
          port = 14100 + n
      copy srcFolder (un Folder folder)
      mkConnection folder port

    let pool = ResourcePool connections
    q <-
      Q.mkQueue
        { maxSize: 50
        , timeout: 60.0 # Seconds # fromDuration
        }
        pool
    serverSetup (makeApp q)

makeApp ∷ Queue PscIdeConnection -> App
makeApp q = do
  use $ static "assets"
  useExternal jsonBodyParser
  E.post "/compileAndRun" (compileAndRunHandler q)

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
