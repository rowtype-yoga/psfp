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
    , measure ∷ String
    , borderThin ∷ String
    }

type YogaTheme
  = ( backgroundColour ∷ String
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
    , highlightColourRotatedForwards ∷ String
    , highlightColourRotatedBackwards ∷ String
    , highlightColour ∷ String
    , highlightColourDark ∷ String
    , altHighlightColour ∷ String
    , altHighlightColourDark ∷ String
    , isLight ∷ Boolean
    , textFontFamily ∷ String
    , headingFontFamily ∷ String
    , codeFontFamily ∷ String
    , yellow ∷ String
    , green ∷ String
    , pink ∷ String
    , orange ∷ String
    , turquoise ∷ String
    , red ∷ String
    , blue ∷ String
    , white ∷ String
    , fontWeightBold ∷ String
    , measure ∷ String
    , borderThin ∷ String
    )

type CSSTheme
  = Record YogaTheme
