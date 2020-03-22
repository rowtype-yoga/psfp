module Yoga.Imposter.Styles where

import Prelude hiding (top)
import CSS (Abs, Size, absolute, declare, fixed, left, maxHeight, maxWidth, pct, position, px, reference, top, transform, translate, unitless, variable, (!*), (!-))
import CSS.Overflow (overflow, overflowAuto)
import Data.Maybe (Maybe)
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Helpers ((?||))
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( margin ∷ Maybe (Size Abs)
    , fixed ∷ Maybe Boolean
    , breakout ∷ Maybe Boolean
    )

type Props
  = Record PropsR

styles ∷ JSSClasses YogaTheme Props ( imposter ∷ JSSElem Props, contain ∷ JSSElem Props )
styles =
  jssClasses \theme ->
    { imposter:
      \props -> do
        position $ (props.fixed <#> if _ then fixed else absolute) ?|| absolute
        top (50.0 # pct)
        left (50.0 # pct)
        transform $ translate (-50.0 # pct) (-50.0 # pct)
    , contain:
      \props -> do
        let
          marginVar = variable "margin" $ props.margin ?|| (0.0 # px)
        declare marginVar
        overflow overflowAuto
        maxWidth $ (100.0 # pct) !- (reference marginVar !* unitless 2.0)
        maxHeight $ (100.0 # pct) !- (reference marginVar !* unitless 2.0)
    }
