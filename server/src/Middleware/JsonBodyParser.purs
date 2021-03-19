module Middleware.JsonBodyParser where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn3)
import Node.Express.Types (Request, Response)

foreign import jsonBodyParser ∷ EffectFn3 Request Response (Effect Unit) Unit
