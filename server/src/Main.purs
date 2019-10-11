module Main where

import Prelude

import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class.Console (info)
import Node.Encoding (Encoding(..))
import Node.Express.App (App, listenHttp, listenHttps, use)

import Node.Express.Middleware.Static (static)
import Node.FS.Sync (readTextFile)
import Node.HTTP (Server)
import Node.Process (lookupEnv)

main ∷ Effect Server
main = do
  serverSetup makeApp

makeApp ∷ App
makeApp = do
  use $ static "assets"
  -- useOnError (errorHandler log)

data CertificatesToUse
  = UseLocalCertificates
  | DoNotUseLocalCertificates

parseCertificatesToUse :: String -> CertificatesToUse
parseCertificatesToUse = case _ of
  "LOCAL" -> UseLocalCertificates
  _ -> DoNotUseLocalCertificates

serverSetup ∷ App -> Effect Server
serverSetup app = do
  maybePortString <- lookupEnv "PORT"
  let port = maybePortString >>= fromString # fromMaybe 14188
  useLocalCertificates <- fromMaybe DoNotUseLocalCertificates <<< map parseCertificatesToUse <$> lookupEnv "CERTS"
  listen <- case useLocalCertificates of
    UseLocalCertificates -> do
      httpsOptions <- makeHttpsOptions
      pure $ listenHttps app (port ∷ Int) httpsOptions
    DoNotUseLocalCertificates ->
      pure $ listenHttp app port
  listen \_ -> info $ "psfp server started on port " <> show port
  where
  makeHttpsOptions = do
    key <- readTextFile UTF8 "server.key"
    cert <- readTextFile UTF8 "server.cert"
    pure { key, cert }
