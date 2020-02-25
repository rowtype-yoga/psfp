module Theme.Default where

import Prelude
import Color as Color
import Data.Maybe (fromMaybe')
import Partial.Unsafe (unsafeCrashWith)
import Theme.Types (Theme)

systemFontStack ∷ Array String
systemFontStack = [ "-apple-system", "BlinkMacSystemFont", "Helvetica", "Arial", "sans-serif" ]

hex :: String -> Color.Color
hex c = c # Color.fromHexString # fromMaybe' \_ -> unsafeCrashWith $ "Invalid hex string " <> c

darkTheme ∷ Theme
darkTheme =
  { backgroundColour: Color.hsl 220.0 0.42 0.13
  , textColour: Color.hsl 225.0 0.28 0.90
  , interfaceColour: Color.hsl 225.0 0.48 0.12
  , highlightColour: Color.rgb 209 51 225
  , altHighlightColour: Color.hsl 84.0 0.617 0.631
  , textFontFamily: [ """Rubik""" ] <> systemFontStack
  , headingFontFamily: [ """Rubik""" ] <> systemFontStack
  , yellow: Color.rgb 255 235 149
  , green: Color.hsl 84.0 0.617 0.631
  , pink: Color.hsl 276.0 0.677 0.745
  , orange: Color.rgb 255 203 139
  , turquoise: Color.rgb 127 219 202
  , red: Color.rgb 255 88 116
  , blue: Color.rgb 130 170 255
  , white: Color.rgb 250 250 250
  }

lightTheme ∷ Theme
lightTheme =
  darkTheme
    { textColour = Color.hsl 225.0 0.18 0.25
    , backgroundColour = Color.hsl 230.0 0.35 0.90
    , interfaceColour = Color.hsl 210.0 0.30 0.80
    , highlightColour = Color.hsl 260.0 0.85 0.58
    , altHighlightColour = Color.hsl 84.0 0.617 0.631
    , green = Color.hsl 84.0 0.617 0.431
    , pink = Color.hsl 276.0 0.677 0.545
    }
