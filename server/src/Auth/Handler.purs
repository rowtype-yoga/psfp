module Auth.Handler where

import Prelude
import Auth.Types (Token(..))
import Data.Array (elem)
import Data.JSDate (now, toISOString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import Data.String.Utils (lines)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.Express.Handler (HandlerM, next)
import Node.Express.Request (getRequestHeader)
import Node.Express.Response (end, setStatus)
import Node.Express.Response as Response
import Node.FS.Aff (appendTextFile, readTextFile)
import Simple.JSON (write)

readAllowedTokens ∷ Aff (Array Token)
readAllowedTokens = do
  text <- readTextFile UTF8 ".authorized-keys"
  pure $ Token <$> lines text

authHandler ∷ Array Token -> HandlerM Unit
authHandler authorizedTokens = do
  authHeader <- getRequestHeader "Authorization"
  let maybeToken = authHeader >>= stripPrefix (Pattern "Bearer ") <#> Token
  ts <- now >>= toISOString # liftEffect
  let log s = liftAff $ appendTextFile UTF8 "access.log" $ ts <> ": " <> s
  case maybeToken of
    Nothing -> do
      setStatus 401
      log $ "Denied request without token"
      Response.send $ write { error: "Jetzt hör'n Sie mir mal zu! Erstens brauch ich ihren Namen!!" }
      end
    Just token@(Token raw)
      | elem token authorizedTokens -> do
        log $ "Access granted to " <> raw
        next
    Just (Token invalid) -> do
      setStatus 403
      log $ "Access denied to " <> invalid
      Response.send $ write { error: "Ich seh' ja ein das man hier der gläserne Mensch ist." }
      end
