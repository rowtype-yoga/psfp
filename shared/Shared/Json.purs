module Shared.Json (readJsonAff, readAff) where

import Prelude

import Data.Either (Either, either)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (Foreign)
import Simple.JSON (class ReadForeign, read, readJSON)

readJsonAff ∷ ∀ a. ReadForeign a => String -> Aff a
readJsonAff = readJSON >>> orThrow

readAff ∷ ∀ a. ReadForeign a => Foreign -> Aff a
readAff = read >>> orThrow

orThrow ∷ ∀ a s. Show s => Either s a -> Aff a
orThrow = either (show >>> throw >>> liftEffect) pure
