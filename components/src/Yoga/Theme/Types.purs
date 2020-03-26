module Yoga.Theme.Types where

import Color (Color)
import Data.Array.NonEmpty (NonEmptyArray)

type Theme
  = { backgroundColour ∷ Color
    , textColour ∷ Color
    , interfaceColour ∷ Color
    , highlightColour ∷ Color
    , altHighlightColour ∷ Color
    , textFontFamily ∷ NonEmptyArray String
    , headingFontFamily ∷ NonEmptyArray String
    , codeFontFamily ∷ NonEmptyArray String
    , yellow ∷ Color
    , green ∷ Color
    , pink ∷ Color
    , orange ∷ Color
    , turquoise ∷ Color
    , red ∷ Color
    , blue ∷ Color
    , white ∷ Color
    , grey ∷ Color
    , measure ∷ String
    , borderThin ∷ String
    }

type YogaTheme
  = ( backgroundColour ∷ Color
    , backgroundColourLighter ∷ Color
    , backgroundColourLightest ∷ Color
    , backgroundColourDarker ∷ Color
    , backgroundColourDarkest ∷ Color
    , interfaceColour ∷ Color
    , interfaceColourDarker ∷ Color
    , interfaceColourDarkest ∷ Color
    , interfaceColourLighter ∷ Color
    , interfaceColourLightest ∷ Color
    , textColourLightest ∷ Color
    , textColourLighter ∷ Color
    , textColour ∷ Color
    , textColourDarker ∷ Color
    , textColourDarkest ∷ Color
    , highlightColourRotatedForwards ∷ Color
    , highlightColourRotatedBackwards ∷ Color
    , highlightColour ∷ Color
    , highlightColourDark ∷ Color
    , altHighlightColour ∷ Color
    , altHighlightColourDark ∷ Color
    , isLight ∷ Boolean
    , textFontFamily ∷ String
    , headingFontFamily ∷ String
    , codeFontFamily ∷ String
    , yellow ∷ Color
    , green ∷ Color
    , pink ∷ Color
    , orange ∷ Color
    , turquoise ∷ Color
    , red ∷ Color
    , blue ∷ Color
    , grey ∷ Color
    , white ∷ Color
    , fontWeightBold ∷ String
    , measure ∷ String
    , borderThin ∷ String
    )

type CSSTheme
  = Record YogaTheme
