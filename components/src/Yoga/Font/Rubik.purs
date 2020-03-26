module Yoga.Font.Rubik (fontFamilies) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Font (Font, FontFamily)

foreign import rubikRegularwoff2 ∷ Font

foreign import rubikMediumwoff2 ∷ Font

foreign import rubikMediumItalicwoff2 ∷ Font

foreign import rubikItalicwoff2 ∷ Font

foreign import rubikLightwoff2 ∷ Font

foreign import rubikLightItalicwoff2 ∷ Font

fontFamilies ∷ Array FontFamily
fontFamilies =
  [ { fontFamily: "Rubik"
    , src: "url(" <> unsafeCoerce rubikItalicwoff2 <> """) format("woff2")"""
    , fontWeight: "normal"
    , fontStyle: "italic"
    }
  , { fontFamily: "Rubik Medium"
    , src: "url(" <> unsafeCoerce rubikMediumwoff2 <> """) format("woff2")"""
    , fontWeight: "400"
    , fontStyle: "normal"
    }
  , { fontFamily: "Rubik"
    , src: "url(" <> unsafeCoerce rubikMediumItalicwoff2 <> """) format("woff2")"""
    , fontWeight: "bold"
    , fontStyle: "italic"
    }
  , { fontFamily: "Rubik"
    , src: "url(" <> unsafeCoerce rubikItalicwoff2 <> """) format("woff2")"""
    , fontWeight: "400"
    , fontStyle: "normal"
    }
  , { fontFamily: "Rubik Light"
    , src: "url(" <> unsafeCoerce rubikLightwoff2 <> """) format("woff2")"""
    , fontWeight: "400"
    , fontStyle: "normal"
    }
  , { fontFamily: "Rubik Light"
    , src: "url(" <> unsafeCoerce rubikLightItalicwoff2 <> """) format("woff2")"""
    , fontWeight: "400"
    , fontStyle: "italic"
    }
  , { fontFamily: "Rubik Regular"
    , src: "url(" <> unsafeCoerce rubikRegularwoff2 <> """) format("woff2")"""
    , fontWeight: "normal"
    , fontStyle: "normal"
    }
  ]
