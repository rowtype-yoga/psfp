module Yoga.FillInTheGaps.Styles where

import Prelude hiding (top)
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ()

type Props
  = Record PropsR

type Classes a
  = ( blue ∷ a
    , green ∷ a
    )

styles ∷ JSSClasses YogaTheme Props (Classes (JSSElem Props))
styles =
  jssClasses \theme ->
    { blue: { color: theme.blue }
    , green: { color: theme.green }
    }
