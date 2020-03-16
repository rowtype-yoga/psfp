module Yoga.Font where

foreign import data Font ∷ Type

type FontFamily
  = { fontFamily ∷ String
    , fontStyle ∷ String
    , fontWeight ∷ String
    , src ∷ String
    }
