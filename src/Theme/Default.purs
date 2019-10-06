module Theme.Default where

import Color as Color
import Theme.Types (Theme)

darkTheme ∷ Theme
darkTheme =
  -- { backgroundColour: Color.hsl 215.0 0.28 0.20
  { backgroundColour: Color.hsl 255.0 0.24 0.13
  , textColour: Color.hsl 225.0 0.28 0.90
  , interfaceColour: Color.hsl 245.0 0.28 0.20
  , highlightColour: Color.rgb 183 130 218
  , altHighlightColour: Color.hsl 84.0 0.617 0.631
  -- , textFontFamily: ["-apple-system", "BlinkMacSystemFont", "Helvetica", "Arial", "sans-serif"]
  , textFontFamily: [ "Montserrat", "Monaco", "sans-serif" ]
  , headingFontFamily: [ "Montserrat", "Monaco", "sans-serif" ]
  , yellow: Color.rgb 255 235 149
  , green: Color.hsl 84.0 0.617 0.631
  , pink: Color.hsl 276.0 0.677 0.745
  , orange: Color.rgb 255 203 139
  , turquoise: Color.rgb 127 219 202
  , red: Color.rgb 255 88 116
  , blue: Color.rgb 130 170 255
  }

lightTheme ∷ Theme
lightTheme =
  darkTheme
    { textColour = Color.hsl 225.0 0.18 0.25
    , backgroundColour = Color.hsl 210.0 0.00 0.98
    , interfaceColour = Color.hsl 210.0 0.10 0.70
    , highlightColour = Color.hsl 260.0 0.85 0.58
    , altHighlightColour = Color.hsl 84.0 0.617 0.631
    , green = Color.hsl 84.0 0.617 0.431
    , pink = Color.hsl 276.0 0.677 0.545
    }
