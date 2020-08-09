module Playground.Playground where

import Prelude
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)

newtype Folder = Folder String

derive instance ntFolder ∷ Newtype Folder _
derive newtype instance eqFolder ∷ Eq Folder

foreign import copyImpl ∷ Fn2 String String (Promise Unit)

copy ∷ String -> String -> Aff Unit
copy src target = runFn2 copyImpl src target # toAff
