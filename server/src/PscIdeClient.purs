module PscIdeClient where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Posix.Signal (Signal(..))
import Data.String.Utils (endsWith)
import Effect.Aff (Aff, Canceler, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.Buffer as Buf
import Node.ChildProcess (ChildProcess, StdIOBehaviour(..))
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Net.Socket (Socket)
import Node.Net.Socket as Socket
import Playground.Playground (Folder(..))
import Simple.JSON (writeJSON)

type BuildCommand
  = { command ∷ String
    , params ∷
      { file ∷ String
      }
    }

loadCommand ∷
  { command ∷ String
  }
loadCommand =
  { command: "load"
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
        log $ "Destroying socket..."
        Socket.destroy s Nothing

startIdeServer ∷ Folder -> Int -> Aff ChildProcess
startIdeServer folder port = do
  log $ "Spawning ide server" <> infoString
  cp <- spawnProcess folder "npx" [ "purs", "ide", "server", "-p", show port, "--editor-mode", "--no-watch" ]
  -- liftEffect $ onDataString (CP.stdout cp) UTF8 \str -> do
  --   log $ "--------" <> infoString <> "\n----------\n" <> str <> "\n----------\n" 
  log $ "Spawned ide server" <> infoString
  -- building once
  { error, stderr, stdout } <- execCommand folder "npx spago build"
  stderrStr <- liftEffect $ Buf.toString UTF8 stderr
  log $ "Built: " <> stderrStr
  log $ "Loading modules" <> infoString
  loadPscIde folder port
  pure cp
  where
  infoString =
    " in folder "
      <> un Folder folder
      <> " on port "
      <> show port

execCommand ∷ Folder -> String -> Aff CP.ExecResult
execCommand folder command =
  makeAff \callback -> do
    let
      options = CP.defaultExecOptions { cwd = Just (un Folder folder) }
    childProcess <- CP.exec command options (callback <<< Right)
    pure $ effectCanceler ((log $ "Killing " <> show (CP.pid childProcess)) *> CP.kill SIGKILL childProcess)

restartIdeServer ∷ Folder -> Int -> Ref ChildProcess -> Aff Unit
restartIdeServer folder port processRef = do
  cp <- Ref.read processRef # liftEffect
  CP.kill SIGKILL cp # liftEffect
  newCp <- startIdeServer folder port
  Ref.write newCp processRef # liftEffect

spawnProcess ∷ Folder -> String -> Array String -> Aff ChildProcess
spawnProcess folder command args =
  makeAff \callback -> do
    let
      options = CP.defaultSpawnOptions { cwd = Just (un Folder folder), stdio = Just <$> [Ignore, Ignore, Ignore] }
    childProcess <- CP.spawn command args options
    callback (Right childProcess)
    pure $ effectCanceler
      $ (log $ "Killing " <> show (CP.pid childProcess))
      *> CP.kill SIGKILL childProcess

newtype PscIdeConnection
  = PscIdeConnection
  { serverProcessRef ∷ Ref ChildProcess
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

loadPscIde ∷ Folder -> Int -> Aff Unit
loadPscIde folder port = do
  makeAff \affCb -> do
    socket <- Socket.createConnectionTCP port "localhost" mempty
    -- maybe timeout?
    Socket.onError socket (affCb <<< Left)
    void
      $ Socket.writeString socket (writeJSON loadCommand <> "\n") UTF8 (affCb (Right unit))
    liftEffect
      $ Socket.onClose socket case _ of
          true -> mempty -- should be covered in onError
          false -> do
            affCb (Right unit)
    let
      command = writeJSON buildCommand <> "\n"
    Socket.onReady socket (void $ Socket.writeString socket command UTF8 mempty)
    pure (closeSocketCanceller socket)

compileCode ∷ String -> PscIdeConnection -> Aff String
compileCode code (PscIdeConnection { port, folder, serverProcessRef }) = do
  serverProcess <- Ref.read serverProcessRef # liftEffect
  saveMainFile folder code
  makeAff \affCb -> do
    log $ "Compiling on " <> show port
    socket <- Socket.createConnectionTCP port "localhost" mempty
    -- Socket.onError socket (affCb <<< Left)
    resultRef <- Ref.new ""
    Socket.onData socket \dataFromServer -> do
      log $ "Data on port " <> show port
      str <- dataFromServer # either (Buf.toString UTF8) pure
      newStr <- resultRef # Ref.modify (_ <> str)
      when (newStr # endsWith "\n") do
        log $ "Enough data on " <> show port <> " ending socket\n"
        void $ Socket.endString socket "" UTF8 mempty
        affCb (Right newStr)
    let
      command = writeJSON buildCommand
    Socket.onReady socket do
      log $ "Socket " <> show port <> " ready"
      void $ Socket.writeString socket (command <> "\n") UTF8 mempty
    pure (closeSocketCanceller socket)

mkConnection ∷ Folder -> Int -> Aff PscIdeConnection
mkConnection folder port = do
  serverProcess <- startIdeServer folder port
  serverProcessRef <- Ref.new serverProcess # liftEffect
  liftEffect
    $ CP.onError serverProcess \e ->
        log $ "Server process on port " <> show port <> " got error " <> show e
  pure
    $ PscIdeConnection
        { serverProcessRef
        , port
        , folder
        }
