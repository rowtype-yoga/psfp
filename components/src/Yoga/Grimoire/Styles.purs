module Yoga.Grimoire.Styles where

import Prelude hiding (top)
import CSS (backgroundColor, borderRadius, height, px)
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ()

type Props
  = Record PropsR

type Classes a
  = ( container ∷ a
    )

styles ∷ JSSClasses YogaTheme Props (Classes (JSSElem Props))
styles =
  jssClasses \theme@{ s0, s_2 } ->
    { container:
      do
        height (100.0 # px)
        borderRadius s_2 s_2 s_2 s_2
        backgroundColor $ theme.interfaceColour
    }
