module Yoga.Font.PragmataPro (fontFamily) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Font (Font, FontFamily)

foreign import ppWoff2 :: Font

fontFamily :: FontFamily
fontFamily =
  { fontFamily: "PragmataPro"
  , src: "url(" <> unsafeCoerce ppWoff2 <> """) format("woff2")"""
  , fontStyle: "normal"
  , fontWeight: "normal"
  }
