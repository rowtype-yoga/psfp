module Yoga.InlineCode.Styles where

import Prelude
import CSS (GenericFontFamily(..), Selector, TimingFunction(..), animation, background, border, borderBox, borderRadius, boxSizing, color, display, element, focus, fontFamily, fontSize, fromString, inlineBlock, iterationCount, keyframes, lineHeight, nil, padding, sec, solid, transform, unitless, width, (!*), (!+), (&), (?))
import CSS.Animation (forwards, normalAnimationDirection)
import CSS.Common (baseline)
import CSS.Overflow (overflow, visible)
import CSS.Size (ch)
import CSS.Transform (scale3d)
import CSS.VerticalAlign (verticalAlign)
import Color as Color
import Data.Array.NonEmpty as NEA
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple.Nested ((/\))
import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Yoga.Helpers ((?||))
import Yoga.Theme (withAlpha)
import Yoga.Theme.Types (YogaTheme, CSSTheme)

type PropsR
  = ( regularFont ∷ Maybe Boolean
    , width ∷ Maybe Int
    )

type Props
  = Record PropsR

styles ∷
  JSSClasses YogaTheme Props
    ( inlinecode ∷ JSSElem Props
    , container ∷ JSSElem Props
    , "@keyframes plop" ∷ JSSElem Props
    )
styles = jssClasses go
  where
  go (theme@{ s0, s1, s_1, s_2, s_3, s_4, s_5 } ∷ CSSTheme) =
    { container:
      do
        display inlineBlock
    , "@keyframes plop":
      keyframes mempty
        $ (0.0 /\ transform (scale3d 0.8 0.8 0.8))
        :| [ 33.3 /\ transform (scale3d 1.1 1.1 1.1)
          , 67.7 /\ transform (scale3d 0.9 0.9 0.9)
          , 86.7 /\ transform (scale3d 1.05 1.05 1.05)
          , 100.0 /\ transform (scale3d 1.0 1.0 1.0)
          ]
    , inlinecode:
      \props ->
        jss { outline: "none" }
          <> jss do
              overflow visible
              boxSizing borderBox
              verticalAlign baseline
              background $ withAlpha 0.15 theme.grey
              border solid s_5 (Color.rgba 0 0 0 0.0)
              borderRadius s_2 s_2 s_2 s_2
              fontFamily
                (NEA.toArray theme.codeFontFamily)
                (NonEmpty (GenericFontFamily $ fromString "monospace") [])
              fontSize (s0)
              lineHeight (s0)
              width $ (props.width ?|| 10 # toNumber # ch) !+ (unitless 4.3 !* s_5)
              padding nil s_5 nil s_5
              color theme.textColour
              this & focus
                ? do
                    animation (fromString "$plop") (0.3 # sec) EaseIn (0.0 # sec) (1.0 # iterationCount) normalAnimationDirection forwards
                    border solid s_5 theme.highlightColourRotatedBackwards
    }

this ∷ Selector
this = element "&"
