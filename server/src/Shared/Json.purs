module Shared.Json (readJsonAff, readAff) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (jsonParser)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either (Either, either)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw, error)

readJsonAff ∷ ∀ a. DecodeJson a => String -> Aff a
readJsonAff s = either (throwError <<< error) pure (jsonParser s) >>= readAff

readAff ∷ ∀ a. DecodeJson a => Json -> Aff a
readAff s = either (throwError <<< error <<< printJsonDecodeError) pure (decodeJson s)

orThrow ∷ ∀ a s. Show s => Either s a -> Aff a
orThrow = either (show >>> throw >>> liftEffect) pure
