module Yoga.Helpers where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)

fromMaybeFlipped ∷ ∀ a. Maybe a -> a -> a
fromMaybeFlipped = flip fromMaybe

infixr 5 fromMaybeFlipped as ?||

ifJustTrue ∷ ∀ m. Monoid m => Maybe Boolean -> m -> m
ifJustTrue option value = if option == Just true then value else mempty

ifJustFalse ∷ ∀ m. Monoid m => Maybe Boolean -> m -> m
ifJustFalse option value = if option == Just false then value else mempty
