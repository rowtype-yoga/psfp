module Node.OS where

import Effect (Effect)

foreign import numCpus ∷ Effect Int
