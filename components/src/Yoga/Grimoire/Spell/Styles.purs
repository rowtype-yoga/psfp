module Yoga.Grimoire.Spell.Styles where

import Prelude hiding (top)
import CSS (backgroundColor, black, boxShadow, color, darken, display, fontFamily, fontSize, fontStyle, fromString, height, inlineBlock, key, pct, rgba, sansSerif, textOverflow, textWhitespace, unitless, white, whitespaceNoWrap, width)
import CSS.FontStyle (italic)
import CSS.Overflow (overflow)
import CSS.Overflow as Overflow
import CSS.Text.Overflow (ellipsis)
import CSS.TextAlign (rightTextAlign, textAlign)
import Data.Array (fromFoldable)
import Data.NonEmpty as NonEmpty
import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Text.Parsing.StringParser.CodePoints (whiteSpace)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ()

type Props
  = Record PropsR

type Classes a
  = ( container ∷ a
    , signature ∷ a
    , description ∷ a
    , card ∷ a
    , name ∷ a
    )

styles ∷ JSSClasses YogaTheme Props (Classes (JSSElem Props))
styles =
  jssClasses \theme@{ s0, s1, s4, s5, s_5 } ->
    { card:
      do
        width (100.0 # pct)
        height (100.0 # pct)
        boxShadow (0.0 # unitless) (0.0 # unitless) (0.0 # unitless) white
        backgroundColor $ rgba 0 0 0 0.0
    , container:
      jss do
        backgroundColor $ rgba 0 0 0 0.0
        width (100.0 # pct)
        (key $ fromString "user-select") "none"
        height (100.0 # pct)
    , signature:
      do
        color theme.textColour
        textAlign rightTextAlign
        fontFamily (fromFoldable theme.codeFontFamily) (NonEmpty.singleton sansSerif)
        fontStyle italic
    , name:
      do
        color theme.highlightColour
        fontFamily (fromFoldable theme.codeFontFamily) (NonEmpty.singleton sansSerif)
        fontSize s1
    , description:
      do
        display inlineBlock
        color theme.grey
        fontStyle italic
        overflow Overflow.hidden
    }
