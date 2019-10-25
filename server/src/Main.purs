module Main where

import Prelude

import Data.Array (unsafeIndex)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Posix.Signal (Signal(..))
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, launchAff_, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info)
import Foreign (unsafeToForeign)
import Middleware.JsonBodyParser (jsonBodyParser)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess (ExecResult, defaultExecOptions, exec, kill)
import Node.Encoding (Encoding(..))
import Node.Express.App (App, listenHttp, listenHttps, use, useExternal)
import Node.Express.App as E
import Node.Express.Handler (HandlerM)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody')
import Node.Express.Response as Response
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.HTTP (Server)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafePartial)
import Shared.Json (readAff)
import Shared.Models.Body (RunResult)
import Shared.Models.Body as Body
import Simple.JSON (read_, write)

toBody ∷ ∀ r m. MonadEffect m => { stdout ∷ Buffer, stderr ∷ Buffer | r } -> m RunResult
toBody result = liftEffect $ ado
  stdout <- Buffer.toString UTF8 result.stdout
  stderr <- Buffer.toString UTF8 result.stderr
  let (code ∷ Maybe Int) = asErrorWithCode result >>= _.code
  in { code, stdout, stderr } ∷ RunResult

type ErrorWithCode = { code ∷ Maybe Int }

asErrorWithCode ∷ ∀ a. a -> Maybe ErrorWithCode
asErrorWithCode = read_ <<< unsafeToForeign

compileCode ∷ String -> Aff String
compileCode code = do
  saveMainFile code
  { stderr } <- execCommand "spago build -- --json-errors"
  errorString <- liftEffect $ Buffer.toString UTF8 stderr
  pure $ unsafePartial (unsafeIndex (lines errorString) 0)

runCode ∷ Aff ExecResult
runCode = execCommand "node run.js"

playground ∷ String
playground = "../playground"

saveMainFile ∷ String -> Aff Unit
saveMainFile code =
  writeTextFile UTF8 (playground <> "/src/Main.purs") code

execCommand ∷ String -> Aff ExecResult
execCommand command =
  makeAff \callback -> do
    let options = defaultExecOptions { cwd = Just playground }
    childProcess <- exec command options (callback <<< Right)
    pure $ effectCanceler (kill SIGKILL childProcess)

compileHandler ∷ HandlerM Unit
compileHandler = do
  body <- getBody'
  json <- readAff body # liftAff
  result <- compileCode (json ∷ Body.CompileRequest).code # liftAff
  Response.send $ write ({ result } ∷ Body.CompileResult)

runHandler ∷ HandlerM Unit
runHandler = do
  result <- liftAff do
    result <- runCode
    toBody result
  Response.send $ write (result ∷ Body.RunResult)

main ∷ Effect Unit
main = launchAff_ do
  serverSetup makeApp

makeApp ∷ App
makeApp = do
  use $ static "assets"
  useExternal jsonBodyParser
  E.post "/compile" compileHandler
  E.post "/run" runHandler
  -- useOnError (errorHandler log)
      -- { method: HTTPure.Post, path: ["_compile"], body } -> do
      --   result <- compileCode body
      --   HTTPure.ok' (HTTPure.header "Content-Type" "application/json") result

      -- { method: HTTPure.Post, path: ["_run"] } -> do
      --   result   <- runCode
      --   response <- toBody result
      --   okJson response

      -- { method: HTTPure.Get, path } | elem path [[], [""], ["/"]] -> do
      --   content <- readTextFile UTF8 "index.html"
      --   HTTPure.ok' (HTTPure.header "Content-Type" "text/html") content

      -- { method: HTTPure.Get, path } -> do
      --   let filePath = intercalate "/" path
      --   let mimeType = guessMimeType filePath
      --   content <- readTextFile UTF8 filePath
      --   HTTPure.ok' (HTTPure.header "Content-Type" mimeType) content

      -- _ -> HTTPure.notFound

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
  let port = maybePortString >>= fromString # fromMaybe 14188
  useLocalCertificates <- fromMaybe DoNotUseLocalCertificates <<< map parseCertificatesToUse <$> lookupEnv "CERTS" # liftEffect
  listen <- case useLocalCertificates of
    UseLocalCertificates -> do
      httpsOptions <- makeHttpsOptions
      pure $ listenHttps app (port ∷ Int) httpsOptions
    DoNotUseLocalCertificates ->
      pure $ listenHttp app port
  liftEffect $ listen \_ -> info $ "psfp server started on port " <> show port
  where
  makeHttpsOptions = do
    key <- readTextFile UTF8 "server.key"
    cert <- readTextFile UTF8 "server.cert"
    pure { key, cert }
