module Main where

import Prelude

import Auth.Handler (authHandler, readAllowedTokens)
import Auth.Types (Token(..))
import Control.Parallel (parOneOf, parTraverse)
import Data.Argonaut (jsonParser)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (elem, (..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either (hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Time.Duration (class Duration, Seconds(..), fromDuration)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, launchAff_, message)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info, log)
import Effect.Exception (Error)
import Foreign (unsafeToForeign)
import JobQueue (EnqueueResult(..), NewJob(..), Queue, ResourcePool(..))
import JobQueue as Q
import Middleware.JsonBodyParser (jsonBodyParser)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess (ExecResult)
import Node.Encoding (Encoding(..))
import Node.Express.App (App, listenHttp, listenHttps, use, useExternal)
import Node.Express.App as E
import Node.Express.Handler (Handler, HandlerM(..), next)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody', getRequestHeader)
import Node.Express.Response (end, setStatus)
import Node.Express.Response as Response
import Node.Express.Types (Request, Response)
import Node.FS.Aff (readTextFile)
import Node.HTTP (Server)
import Node.OS (numCpus)
import Node.Process (lookupEnv)
import Playground.Playground (Folder(..), copy)
import PscIdeClient (PscIdeConnection, compileCode, getFolder, mkConnection, execCommand)
import Shared.Json (readAff, readJsonAff)
import Shared.Models.Body (CompileRequest, RunResult, CompileResult)
import Shared.Models.Body as Body
import Unsafe.Coerce (unsafeCoerce)

toBody ∷ ∀ r m. MonadEffect m => { stdout ∷ Buffer, stderr ∷ Buffer | r } -> m RunResult
toBody result =
  liftEffect
    $ ado
        stdout <- Buffer.toString UTF8 result.stdout
        stderr <- Buffer.toString UTF8 result.stderr
        let (code ∷ Maybe Int) = asErrorWithCode result >>= _.code
        in { code, stdout, stderr } ∷ RunResult

type ErrorWithCode =
  { code ∷ Maybe Int }

asErrorWithCode ∷ ∀ a. a -> Maybe ErrorWithCode
asErrorWithCode = unsafeCoerce >>> decodeJson >>> hush

runCode ∷ ∀ d. Duration d => d -> Folder -> Aff (Maybe ExecResult)
runCode timeout folder =
  parOneOf
    [ Just <$> execCommand folder "exec node run.js"
    , Nothing <$ delay (timeout # fromDuration)
    ]

compileAndRunJob ∷ CompileRequest -> (Handler -> Aff Unit) -> NewJob PscIdeConnection
compileAndRunJob json handle =
  NewJob \jobId conn -> do
    stringOrErr ∷ Either Error String <- attempt $ compileCode json.code conn
    jsonOrErr ∷ Either Error (Either Error CompileResult) <- attempt $ readJsonAff `traverse` stringOrErr
    case jsonOrErr of
      Left e -> do
        handle $ setStatus 500
        log $ "Aff failed with " <> message e
        handle $ Response.send $ encodeJson {}
      Right (Right res)
        | res.resultType == "error" -> do
          handle $ setStatus 422
          handle $ Response.send $ encodeJson res
      Right (Right res) -> do
        runResult <- runCode timeout (getFolder conn)
        case runResult of
          Nothing -> do
            handle $ setStatus 408
            handle $ Response.send $ encodeJson { error: "Timed out after running for " <> show timeout }
          Just rr -> do
            resultBody <- toBody rr
            handle $ Response.send $ encodeJson (resultBody ∷ Body.RunResult)
      Right (Left errs) -> do
        handle $ setStatus 500
        log $ "Could not decode: " <> show (lmap (const "no way") stringOrErr) <> "\nErrors: " <> show errs
        handle $ Response.send $ encodeJson {}
  where
  timeout = 1.0 # Seconds

compileAndRunHandler ∷ Queue PscIdeConnection -> Handler
compileAndRunHandler queue = do
  body <- getBody'
  json <- readAff (unsafeCoerce body) # liftAff
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
          Response.send $ encodeJson { error: "Queue full" }

unHandler ∷ ∀ a. Request -> Response -> Effect Unit -> HandlerM a -> Aff a
unHandler req res next (HandlerM h) = h req res next

srcFolder ∷ String
srcFolder = "../playground"

destFolder ∷ String
destFolder = "../playgrounds/"

main ∷ Effect Unit
main = do
  launchAff_ do
    tokens <- readAllowedTokens
    cpus <- numCpus # liftEffect
    let
      poolSize = max 2 (cpus / 2) -- use at least 2
      mkFolder = Folder <<< (destFolder <> _) <<< show
    connections <-
      (1 .. poolSize)
        # parTraverse \n -> do
            let
              folder = mkFolder n
              port = 14100 + n
            log $ "Copying to folder " <> (un Folder folder)
            copy srcFolder (un Folder folder)
            mkConnection folder port
    let pool = ResourcePool connections
    q <-
      Q.mkQueue
        { maxSize: 50
        , timeout: 10.0 # Seconds # fromDuration
        }
        pool
    serverSetup (makeApp tokens q)

makeApp ∷ Array Token -> Queue PscIdeConnection -> App
makeApp tokens q = do
  use $ static "assets"
  useExternal jsonBodyParser
  use (authHandler tokens)
  E.post "/compileAndRun" (compileAndRunHandler q)

serverSetup ∷ App -> Aff Server
serverSetup app = do
  maybePortString <- lookupEnv "PORT" # liftEffect
  let port = maybePortString >>= fromString # fromMaybe 14188
  liftEffect $ (listenHttp app port) \_ -> info $ "psfp server started on port " <> show port
  where
  makeHttpsOptions = do
    key <- readTextFile UTF8 "server.key"
    cert <- readTextFile UTF8 "server.cert"
    pure { key, cert }
