module Yoga.ClickAway.Styles where

import Prelude hiding (top)
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ()

type Props
  = Record PropsR

type Classes a
  = ( darkOverlay ∷ a )

styles ∷ JSSClasses YogaTheme Props (Classes (JSSElem Props))
styles =
  jssClasses \theme ->
    { darkOverlay:
      { width: "100vw"
      , height: "100vh"
      , backdropFilter: "blur(6px) brightness(67%)"
      }
    }
