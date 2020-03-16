module Yoga.Center.Styles where

import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ()

type Props
  = Record PropsR

styles ∷ JSSClasses YogaTheme Props ( centre ∷ JSSElem Props )
styles =
  jssClasses \_ ->
    { centre:
      jss
        { display: "flex"
        , placeContent: "center"
        , minHeight: "100vh"
        }
    }
