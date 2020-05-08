module Yoga.Grimoire.Spell.Styles where

import Prelude hiding (top)
import CSS (backgroundColor, color, fontFamily, fontSize, fontStyle, height, pct, sansSerif, width)
import CSS.FontStyle (italic)
import CSS.TextAlign (rightTextAlign, textAlign)
import Data.Array (fromFoldable)
import Data.NonEmpty as NonEmpty
import JSS (JSSClasses, JSSElem, jss, jssClasses)
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
    , container:
      jss { userSelect: "none" }
        <> jss do
            backgroundColor theme.backgroundColourLighter
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
        color theme.grey
        fontStyle italic
    }
