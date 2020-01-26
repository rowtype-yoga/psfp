module PscIdeClient where

import Prelude

import Control.Monad.Rec.Class (untilJust)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (for_, intercalate)
import Data.Int (fromString)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Posix.Signal (Signal(..))
import Data.Semigroup.Foldable (intercalateMap)
import Data.String as Str
import Data.String.Utils (lines)
import Data.Time.Duration (Milliseconds(..), Seconds(..), fromDuration)
import Data.Traversable (for)
import Data.UUID as UUID
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Fiber, delay, effectCanceler, launchAff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info, log, logShow)
import Effect.Class.Console as Console
import Effect.Exception (Error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (renderForeignError, unsafeToForeign)
import JobQueue (EnqueueResult(..), JobId(..), NewJob(..), Queue, ResourcePool(..))
import JobQueue as Q
import Middleware.JsonBodyParser (jsonBodyParser)
import Node.Buffer as Buf
import Node.ChildProcess (ChildProcess, defaultExecOptions)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.Express.Handler (HandlerM(..), Handler)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody')
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.HTTP (Server)
import Node.Net.Socket (Socket)
import Node.Net.Socket as Socket
import Node.OS (numCpus)
import Node.Process (lookupEnv)
import Playground.Playground (Folder(..))
import Shared.Models.Body (CompileResult)
import Simple.JSON (readJSON, writeJSON)

type BuildCommand
  = { command ∷ String
    , params ∷
      { file ∷ String
      }
    }

buildCommand ∷ BuildCommand
buildCommand =
  { "command": "rebuild"
  , "params": { "file": "/Users/mark/code/psfp/playground/src/Main.purs" }
  }

closeSocketCanceller :: Socket -> Canceler
closeSocketCanceller s =
  effectCanceler $
    unlessM
    (Socket.destroyed s) do
        -- Socket.end s (pure unit)
        log $ "Destroying socket..."
        Socket.destroy s Nothing

createConnectionTCP ∷ Int -> Aff Socket
createConnectionTCP port =
  makeAff \callback -> do
    socket <- Socket.createConnectionTCP port "localhost" mempty
    Socket.onReady socket (callback (Right socket))
    pure $ closeSocketCanceller socket

splitter :: Ref String -> String -> Effect (Array String)
splitter chunkRef str = case NEA.fromArray splitAtNewlines of
  Nothing -> pure []
  Just newChunks -> do
    chunk <- Ref.read chunkRef
    let
      { init, last } = NEA.unsnoc newChunks
      newTail = Array.drop 1 init
      oldChunkAndNewFirst = NEA.head newChunks <> chunk
    Ref.write last chunkRef
    pure (Array.cons oldChunkAndNewFirst newTail)
  where
  splitAtNewlines = Str.split (Str.Pattern "\n") str

writeString :: String -> Socket -> Aff Unit
writeString str socket = 
  makeAff \callback -> do
    void $ Socket.writeString socket str UTF8 (callback (Right unit))
    pure $ closeSocketCanceller socket

onError :: (Error -> Effect Unit) -> Socket -> Effect Unit
onError = flip Socket.onError

onData ∷ Socket -> (String -> Effect Unit) -> Effect Unit
onData socket callback = do
    chunkRef <- Ref.new ""
    Socket.onData socket case _ of
      Left buffer -> do
          response <- Buf.toString UTF8 buffer
          results <- splitter chunkRef response
          for_ results callback
      Right response -> do
          results <- splitter chunkRef response
          for_ results callback

startIdeServer :: Folder -> Int -> Aff ChildProcess
startIdeServer folder port = 
  spawnProcess folder ("npx purs ide server -p " <> show port)

restartIdeServer :: _ -> _ -> _ -> Aff Unit
restartIdeServer folder port processRef = do
  cp <- Ref.read processRef # liftEffect
  CP.kill SIGKILL cp # liftEffect
  newCp <- startIdeServer folder port
  Ref.write newCp processRef # liftEffect

spawnProcess :: Folder -> String -> Aff ChildProcess
spawnProcess folder command =
  makeAff \callback -> do
    let
      options = CP.defaultExecOptions { cwd = Just (un Folder folder) }
    childProcess <- CP.exec command options mempty
    callback (Right childProcess)
    pure $ effectCanceler ((log $ "Killing " <> show (CP.pid childProcess)) *> CP.kill SIGKILL childProcess)

newtype PscIdeConnection = PscIdeConnection
  { serverProcess :: ChildProcess
  , socket :: Socket
  , port :: Int
  , folder :: Folder
  , answersRef :: Ref (Array CompileResult)
  }

derive instance ntPscIdeConnection :: Newtype PscIdeConnection _
instance eqPscIdeConnection :: Eq PscIdeConnection where
  eq c1 c2 = (unwrap c1).port == (unwrap c2).port

compileCode :: String -> PscIdeConnection -> Aff CompileResult
compileCode code conn@(PscIdeConnection { serverProcess, socket, port, folder, answersRef }) = do
  saveMainFile folder code
  writeString (writeJSON buildCommand <> "\n") socket
  untilJust do
    delay (50.0 # Milliseconds)
    popOldestAnswer conn # liftEffect
    
saveMainFile ∷ Folder -> String -> Aff Unit
saveMainFile folder code = writeTextFile UTF8 (un Folder folder <> "/src/Main.purs") code

popOldestAnswer :: PscIdeConnection -> Effect (Maybe CompileResult)
popOldestAnswer (PscIdeConnection { answersRef }) = do
  result <- Ref.read answersRef <#> Array.head
  Ref.modify_ (Array.drop 1) answersRef
  pure result

getFolder :: PscIdeConnection -> Folder
getFolder (PscIdeConnection { folder }) = folder

mkConnection :: Folder -> Int -> Aff PscIdeConnection
mkConnection folder port = do
  serverProcess <- startIdeServer folder port
  delay (2.0 # Seconds # fromDuration)
  socket <- createConnectionTCP port
  answersRef <- Ref.new [] # liftEffect
  onData socket (parseAndAppend answersRef) # liftEffect
  pure $ PscIdeConnection 
    { serverProcess, socket, port, folder, answersRef }
  -- onClose ???

  where
    parseAndAppend :: _ -> String -> Effect Unit
    parseAndAppend answersRef string =
      case readJSON (spy "shit" string) of
        Left errs -> log $ intercalate "\n" (errs <#> renderForeignError)
        Right more -> Ref.modify_ (_ <> [more]) answersRef
        
        

-- testRun ∷ Effect Unit
-- testRun = do
--   socket <- Socket.createConnectionTCP 14189 "localhost" mempty
--   Socket.onReady socket do
--     log "Ready"
--     log $ "Sending " <> writeJSON buildCommand
--     void $ Socket.writeString socket (writeJSON buildCommand <> "\n") UTF8 mempty
--   Socket.onData socket (onData socket)
--   where
--   onData socket = case _ of
--     Left buffer -> do
--       bufferString <- Buffer.toString UTF8 buffer
--       logShow { _message: "Converted to a `String`", bufferString }
--     Right string -> do
--       logShow { _message: "Received some data", string }
