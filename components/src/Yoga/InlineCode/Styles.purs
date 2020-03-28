module Yoga.InlineCode.Styles where

import Prelude
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( 
    )

type Props
  = Record PropsR

styles ∷
  JSSClasses YogaTheme Props
    ( inlinecode ∷ JSSElem Props
    )
styles =
  jssClasses \theme ->
    { inlinecode:
      \props ->
        {}
    }
