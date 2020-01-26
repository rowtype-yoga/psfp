module PscIdeClient where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Posix.Signal (Signal(..))
import Data.String.Utils (endsWith)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (message)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (renderForeignError)
import Node.Buffer as Buf
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Net.Socket (Socket)
import Node.Net.Socket as Socket
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
  , "params": { "file": "src/Main.purs" }
  }

closeSocketCanceller ∷ Socket -> Canceler
closeSocketCanceller s =
  effectCanceler
    $ unlessM
        (Socket.destroyed s) do
        -- Socket.end s (pure unit)
        log $ "Destroying socket..."
        Socket.destroy s Nothing

startIdeServer ∷ Folder -> Int -> Aff ChildProcess
startIdeServer folder port = do
  log
    $ "Spawning ide server in folder "
    <> un Folder folder
    <> " on port "
    <> show port
  cp <- spawnProcess folder ("npx purs ide server -p " <> show port)
  log
    $ "Spawned ide server in folder "
    <> un Folder folder
    <> " on port "
    <> show port
  pure cp

restartIdeServer ∷ Folder -> Int -> Ref ChildProcess -> Aff Unit
restartIdeServer folder port processRef = do
  cp <- Ref.read processRef # liftEffect
  CP.kill SIGKILL cp # liftEffect
  newCp <- startIdeServer folder port
  Ref.write newCp processRef # liftEffect

spawnProcess ∷ Folder -> String -> Aff ChildProcess
spawnProcess folder command =
  makeAff \callback -> do
    let
      options = CP.defaultExecOptions { cwd = Just (un Folder folder) }
    childProcess <- CP.exec command options mempty
    callback (Right childProcess)
    pure $ effectCanceler ((log $ "Killing " <> show (CP.pid childProcess)) *> CP.kill SIGKILL childProcess)

newtype PscIdeConnection
  = PscIdeConnection
  { serverProcess ∷ ChildProcess
  , port ∷ Int
  , folder ∷ Folder
  }

derive instance ntPscIdeConnection :: Newtype PscIdeConnection _
instance eqPscIdeConnection :: Eq PscIdeConnection where
  eq c1 c2 = (unwrap c1).port == (unwrap c2).port

saveMainFile ∷ Folder -> String -> Aff Unit
saveMainFile folder code = writeTextFile UTF8 (un Folder folder <> "/src/Main.purs") code

getFolder ∷ PscIdeConnection -> Folder
getFolder (PscIdeConnection { folder }) = folder

compileCode ∷ String -> PscIdeConnection -> Aff String
compileCode code (PscIdeConnection { port, folder }) = do
  saveMainFile folder code
  makeAff \affCb -> do
    log $ 
      "Compiling code in " <> (un Folder folder) <> " on port " <> show port
    socket <- Socket.createConnectionTCP port "localhost" mempty
    -- maybe timeout?
    Socket.onError socket (affCb <<< Left)
    resultRef <- Ref.new ""
    Socket.onData socket \dataFromServer -> do
      str <- dataFromServer # either (Buf.toString UTF8) pure
      newStr <- resultRef # Ref.modify (_ <> str)
      when (newStr # endsWith "\n") do
        Socket.endString socket "" UTF8 mempty
    liftEffect
      $ Socket.onClose socket case _ of
          true -> mempty -- should be covered in onError
          false -> do
            res <- Ref.read resultRef
            affCb (Right res)
    let
      command = writeJSON buildCommand <> "\n"
    Socket.onReady socket (void $ Socket.writeString socket command UTF8 mempty)
    pure (closeSocketCanceller socket)

mkConnection ∷ Folder -> Int -> Aff PscIdeConnection
mkConnection folder port = do
  serverProcess <- startIdeServer folder port
  liftEffect $ CP.onError serverProcess \e ->
    log $ "Server process on port " <> show port <> " got error " <> show e
  pure
    $ PscIdeConnection
        { serverProcess
        , port
        , folder
        }