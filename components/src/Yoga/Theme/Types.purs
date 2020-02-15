module Theme.Types where

import Color (Color)

type Theme
  = { backgroundColour ∷ Color
    , textColour ∷ Color
    , interfaceColour ∷ Color
    , highlightColour ∷ Color
    , altHighlightColour ∷ Color
    , textFontFamily ∷ Array String
    , headingFontFamily ∷ Array String
    , yellow ∷ Color
    , green ∷ Color
    , pink ∷ Color
    , orange ∷ Color
    , turquoise ∷ Color
    , red ∷ Color
    , blue ∷ Color
    }

type CSSTheme
  = { backgroundColour ∷ String
    , backgroundColourLighter ∷ String
    , backgroundColourLightest ∷ String
    , backgroundColourDarker ∷ String
    , backgroundColourDarkest ∷ String
    , interfaceColour ∷ String
    , interfaceColourDarker ∷ String
    , interfaceColourDarkest ∷ String
    , interfaceColourLighter ∷ String
    , interfaceColourLightest ∷ String
    , textColourLightest ∷ String
    , textColourLighter ∷ String
    , textColour ∷ String
    , textColourDarker ∷ String
    , textColourDarkest ∷ String
    , highlightColour ∷ String
    , highlightColourDark ∷ String
    , altHighlightColour ∷ String
    , altHighlightColourDark ∷ String
    , isLight ∷ Boolean
    , textFontFamily ∷ String
    , headingFontFamily ∷ String
    , yellow ∷ String
    , green ∷ String
    , pink ∷ String
    , orange ∷ String
    , turquoise ∷ String
    , red ∷ String
    , blue ∷ String
    }
