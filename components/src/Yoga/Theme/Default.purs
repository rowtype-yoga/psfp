module Yoga.Theme.Default where

import Prelude
import CSS (rem)
import Color as Color
import Data.Array.NonEmpty (cons')
import Data.Maybe (fromMaybe')
import JSS (JSSElem, jss)
import Partial.Unsafe (unsafeCrashWith)
import Yoga.Font.Rubik as Rubik
import Yoga.Font.VictorMono as VictorMono
import Yoga.Theme.Types (Theme)

systemFontStack ∷ Array String
systemFontStack = [ "-apple-system", "BlinkMacSystemFont", "Helvetica", "Arial", "sans-serif" ]

hex ∷ String -> Color.Color
hex c = c # Color.fromHexString # fromMaybe' \_ -> unsafeCrashWith $ "Invalid hex string " <> c

fontFaces ∷ JSSElem {}
fontFaces = jss (Rubik.fontFamilies <> VictorMono.fontFamilies)

darkTheme ∷ Theme
darkTheme =
  { backgroundColour: Color.hsl 238.0 0.18 0.20
  , textColour: Color.hsl 225.0 0.28 0.90
  , interfaceColour: Color.hsl 235.0 0.22 0.28
  , highlightColour: Color.hsl 285.0 0.88 0.72
  , altHighlightColour: Color.hsl 84.0 0.617 0.631
  , textFontFamily: cons' "Inter" systemFontStack
  , headingFontFamily: cons' "Inter" systemFontStack
  , codeFontFamily: cons' "VictorMono" []
  , yellow: Color.hsl 36.0 0.82 0.76
  , green: Color.hsl 84.0 0.617 0.631
  , purple: Color.hsl 276.0 0.677 0.745
  , red: Color.rgb 255 88 116
  , blue: Color.rgb 130 170 255
  , grey: Color.rgb 150 150 150
  , white: Color.rgb 250 250 250
  , measure: "60ch"
  , borderThin: "0.125rem"
  , ratio: 1.5
  , s0: 1.0 # rem
  }

lightTheme ∷ Theme
lightTheme =
  darkTheme
    { textColour = Color.hsl 220.0 0.18 0.30
    , backgroundColour = Color.hsl 20.0 0.35 0.95
    , interfaceColour = Color.hsl 20.0 0.20 0.975
    , highlightColour = Color.hsl 350.0 0.93 0.67
    , altHighlightColour = Color.hsl 84.0 0.617 0.631
    , green = Color.hsl 170.0 0.39 0.40
    , purple = Color.hsl 284.0 0.64 0.60
    , red = Color.hsl 348.0 0.80 0.58
    , yellow = Color.hsl 36.0 0.82 0.46
    , blue = Color.hsl 221.0 0.92 0.65
    , grey = Color.hsl 270.0 0.10 0.60
    }
