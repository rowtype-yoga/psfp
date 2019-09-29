module Theme.Default where

import Color as Color
import Theme.Types (Theme)

lightTheme ∷ Theme
lightTheme =
  { textColour:         Color.hsl 225.0 0.18 0.25
  , backgroundColour:   Color.hsl 210.0 0.00 0.98
  , interfaceColour:    Color.hsl 210.0 0.10 0.70
  , highlightColour:    Color.hsl 150.0 0.55 0.78
  , altHighlightColour: Color.hsl 84.0 0.617 0.631
  , textFontFamily: ["-apple-system", "BlinkMacSystemFont", "Helvetica", "Arial", "sans-serif"]
  , headingFontFamily: ["Montserrat", "Monaco", "sans-serif"]
  , yellow:    Color.rgb 255 235 149
  , green:     Color.rgb 173 219 103
  , pink:      Color.rgb 199 146 234
  , orange:    Color.rgb 255 203 139
  , turquoise: Color.rgb 127 219 202
  , red:       Color.rgb 255 88 116
  , blue:      Color.rgb 130 170 255
  }

darkTheme ∷ Theme
darkTheme =
  { backgroundColour:   Color.hsl 215.0 0.28 0.20
  , textColour:         Color.hsl 225.0 0.28 0.90
  , interfaceColour:    Color.hsl 215.0 0.28 0.20
  , highlightColour:    Color.rgb 183 130 218
  , altHighlightColour: Color.hsl 84.0 0.617 0.631
  , textFontFamily: ["-apple-system", "BlinkMacSystemFont", "Helvetica", "Arial", "sans-serif"]
  , headingFontFamily: ["Montserrat", "Monaco", "sans-serif"]
  , yellow:    Color.rgb 255 235 149
  , green:     Color.rgb 173 219 103
  , pink:      Color.rgb 199 146 234
  , orange:    Color.rgb 255 203 139
  , turquoise: Color.rgb 127 219 202
  , red:       Color.rgb 255 88 116
  , blue:      Color.rgb 130 170 255
  }
