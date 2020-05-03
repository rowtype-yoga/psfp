module Yoga.Grimoire.Styles where

import Prelude hiding (top)
import CSS (backgroundColor, borderRadius)
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
  jssClasses \theme@{ s0 } ->
    { container:
      do
        borderRadius s0 s0 s0 s0
        backgroundColor theme.backgroundColourDarker
    }
