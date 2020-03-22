module Yoga.Font.VictorMono (fontFamilies) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Font (Font, FontFamily)

foreign import victorMonoMediumWoff2 ∷ Font

foreign import victorMonoMediumItalicWoff2 ∷ Font

foreign import victorMonoBoldWoff2 ∷ Font

foreign import victorMonoBoldItalicWoff2 ∷ Font

fontFamilies ∷ Array FontFamily
fontFamilies =
  [ { fontFamily: "VictorMono"
    , src: "url(" <> unsafeCoerce victorMonoMediumWoff2 <> """) format("woff2")"""
    , fontStyle: "normal"
    , fontWeight: "normal"
    }
  , { fontFamily: "VictorMono"
    , src: "url(" <> unsafeCoerce victorMonoBoldWoff2 <> """) format("woff2")"""
    , fontStyle: "normal"
    , fontWeight: "bold"
    }
  , { fontFamily: "VictorMono"
    , src: "url(" <> unsafeCoerce victorMonoMediumItalicWoff2 <> """) format("woff2")"""
    , fontStyle: "italic"
    , fontWeight: "normal"
    }
  , { fontFamily: "VictorMono"
    , src: "url(" <> unsafeCoerce victorMonoBoldItalicWoff2 <> """) format("woff2")"""
    , fontStyle: "italic"
    , fontWeight: "bold"
    }
  ]
