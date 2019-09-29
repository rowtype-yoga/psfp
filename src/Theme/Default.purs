module Theme.Default where

import Color as Color
import Theme.Types (Theme)

darkTheme ∷ Theme
darkTheme =
  { backgroundColour:        Color.hsl 225.0 0.28 0.20 -- hsl(100, 10%, 50%)
  , foregroundColour:        Color.hsl 220.0 0.22 0.90
  , highlightColour:         Color.rgb 0xb7 0x82 0xda
  , altHighlightColour:      Color.hsl 84.0 0.617 0.631
  , textFontFamily: ["'Montserrat'", "-apple-system", "BlinkMacSystemFont", "sans-serif"]
  , headingFontFamily: ["'Montserrat'", "sans-serif"]
  , yellow: Color.rgb 0xFF 0xEB 0x95
  , green: Color.rgb 0xad 0xdb 0x67
  , pink: Color.rgb 0xc7 0x92 0xea
  , orange: Color.rgb 0xff 0xcb 0x8b
  , turquoise: Color.rgb 0x7f 0xdb 0xca
  , red: Color.rgb 0xff 0x58 0x74
  , blue: Color.rgb 0x82 0xAA 0xFF
  }
