module Yoga.Theme.Types where

import CSS (CSSVariable, Rel, Size)
import CSS.Size (Calc)
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
    , purple ∷ Color
    , red ∷ Color
    , blue ∷ Color
    , white ∷ Color
    , grey ∷ Color
    , measure ∷ String
    , borderThin ∷ String
    , ratio ∷ Number
    , s0 ∷ Size Rel
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
    , textFontFamily ∷ NonEmptyArray String
    , headingFontFamily ∷ NonEmptyArray String
    , codeFontFamily ∷ NonEmptyArray String
    , yellow ∷ Color
    , green ∷ Color
    , purple ∷ Color
    , red ∷ Color
    , blue ∷ Color
    , grey ∷ Color
    , white ∷ Color
    , fontWeightBold ∷ String
    , measure ∷ String
    , borderThin ∷ String
    , ratioVar ∷ CSSVariable (Size Calc)
    , s_5Var ∷ CSSVariable (Size Calc)
    , s_4Var ∷ CSSVariable (Size Calc)
    , s_3Var ∷ CSSVariable (Size Calc)
    , s_2Var ∷ CSSVariable (Size Calc)
    , s_1Var ∷ CSSVariable (Size Calc)
    , s0Var ∷ CSSVariable (Size Rel)
    , s1Var ∷ CSSVariable (Size Calc)
    , s2Var ∷ CSSVariable (Size Calc)
    , s3Var ∷ CSSVariable (Size Calc)
    , s4Var ∷ CSSVariable (Size Calc)
    , s5Var ∷ CSSVariable (Size Calc)
    , s0 ∷ Size Rel
    , ratio ∷ Size Calc
    , s_5 ∷ Size Calc
    , s_4 ∷ Size Calc
    , s_3 ∷ Size Calc
    , s_2 ∷ Size Calc
    , s_1 ∷ Size Calc
    , s1 ∷ Size Calc
    , s2 ∷ Size Calc
    , s3 ∷ Size Calc
    , s4 ∷ Size Calc
    , s5 ∷ Size Calc
    )

type CSSTheme
  = Record YogaTheme
