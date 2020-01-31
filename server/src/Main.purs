module Main where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array ((..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (un)
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_, message)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info, log)
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
import PscIdeClient (PscIdeConnection, compileCode, getFolder, mkConnection, execCommand)
import Shared.Json (readAff)
import Shared.Models.Body (CompileRequest, RunResult, CompileResult)
import Shared.Models.Body as Body
import Simple.JSON (readJSON, read_, write)

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

runCode ∷ Folder -> Aff ExecResult
runCode folder = execCommand folder "node run.js"

compileAndRunJob ∷ CompileRequest -> (Handler -> Aff Unit) -> NewJob PscIdeConnection
compileAndRunJob json handle =
  NewJob \jobId conn -> do
    stringOrErr <- attempt $ compileCode json.code conn
    case ((readJSON <$> stringOrErr) :: _ _ (_ _ CompileResult)) of 
      Left e -> do
        handle $ setStatus 500
        log $ "Aff failed with " <> message e
        handle $ Response.send $ write {}
      Right (Right res) | res.resultType == "error" -> do
        handle $ setStatus 422
        handle $ Response.send $ write res
      Right (Right res) -> do
        runResult <- runCode (getFolder conn)
        resultBody <- toBody runResult
        handle $ Response.send $ write (resultBody ∷ Body.RunResult)
      Right (Left errs) -> do
        handle $ setStatus 500
        log $ "Could not decode: " <> show (lmap (const "no way") stringOrErr) <> "\nErrors: " <> show errs
        handle $ Response.send $ write {}

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
      poolSize = max 2 (cpus / 2) -- use at least 2

      mkFolder = Folder <<< (destFolder <> _) <<< show

    connections <- (1 .. poolSize) # parTraverse \n -> do
      let folder = mkFolder n
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
