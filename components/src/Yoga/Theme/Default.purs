module Yoga.Theme.Default where

import Prelude
import CSS (rem)
import Color as Color
import Data.Array.NonEmpty (cons')
import Data.Maybe (fromMaybe')
import Partial.Unsafe (unsafeCrashWith)
import Yoga.Theme.Types (Theme)

systemFontStack ∷ Array String
systemFontStack = [ "-apple-system", "BlinkMacSystemFont", "Helvetica", "Arial", "sans-serif" ]

hex ∷ String -> Color.Color
hex c = c # Color.fromHexString # fromMaybe' \_ -> unsafeCrashWith $ "Invalid hex string " <> c

darkTheme ∷ Theme
darkTheme =
  { backgroundColour: Color.hsl 228.0 0.12 0.20
  , textColour: Color.hsl 225.0 0.28 0.90
  , interfaceColour: Color.hsl 225.0 0.48 0.12
  , highlightColour: Color.hsl 300.0 0.58 0.77
  -- , highlightColour: Color.hsl 33.0 0.37 0.8
  , altHighlightColour: Color.hsl 84.0 0.617 0.631
  , textFontFamily: cons' "Rubik" systemFontStack
  , headingFontFamily: cons' "Rubik" systemFontStack
  , codeFontFamily: cons' "VictorMono" []
  , yellow: Color.rgb 255 235 149
  , green: Color.hsl 84.0 0.617 0.631
  , pink: Color.hsl 276.0 0.677 0.745
  , orange: Color.rgb 255 203 139
  , turquoise: Color.rgb 127 219 202
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
    { textColour = Color.hsl 225.0 0.18 0.25
    -- , backgroundColour = Color.hsl 210.0 0.08 0.87
    , backgroundColour = Color.hsl 30.0 0.50 0.96
    , interfaceColour = Color.hsl 210.0 0.10 0.89
    -- , highlightColour = Color.hsl 209.0 0.95 0.69
    , highlightColour = Color.hsl 350.0 0.90 0.70
    , altHighlightColour = Color.hsl 84.0 0.617 0.631
    , green = Color.hsl 84.0 0.617 0.431
    , pink = Color.hsl 276.0 0.677 0.545
    }
