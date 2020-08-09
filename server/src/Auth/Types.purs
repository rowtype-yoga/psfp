module Auth.Types where

import Prelude

newtype Token = Token String

derive instance eqToken ∷ Eq Token
