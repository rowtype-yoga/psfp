module Yoga.Highlighter.Types where

import Effect.Aff (Aff)

data Language
  = Purescript

newtype HTMLString
  = HTMLString String

type Highlighter r
  = ( highlight ∷
      String -> Language -> Aff HTMLString
    | r
    )
