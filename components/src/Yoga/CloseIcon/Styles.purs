module Yoga.CloseIcon.Styles where

import Prelude hiding (top)
import CSS (rem)
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( 
    )

type Props
  = Record PropsR

type Classes a
  = ( closeIcon ∷ a
    )

styles ∷ JSSClasses YogaTheme Props (Classes (JSSElem Props))
styles =
  jssClasses \theme ->
    let
      boxBorderRadius = 0.33 # rem
    in
      { closeIcon:
        { stroke: theme.textColour
        , strokeWidth: "var(--s0)"
        , width: "var(--s1)"
        , height: "var(--s1)"
        , margin: 0
        , padding: 0
        }
      }
