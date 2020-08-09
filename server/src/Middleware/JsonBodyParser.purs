module Middleware.JsonBodyParser where

import Prelude
import Data.Function.Uncurried (Fn3)
import Effect (Effect)
import Node.Express.Types (Request, Response)

foreign import jsonBodyParser ∷ Fn3 Request Response (Effect Unit) (Effect Unit)
