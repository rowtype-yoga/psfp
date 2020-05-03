module Yoga.Grid.Styles where

import Prelude hiding (top)
import Data.Interpolate (i)
import Data.Maybe (Maybe)
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Helpers ((?||))
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( minWidth ∷ Maybe String
    )

type Props
  = Record PropsR

type Classes a
  = ( grid ∷ a
    )

styles ∷ JSSClasses YogaTheme Props (Classes (JSSElem Props))
styles =
  jssClasses \theme@{ s0 } ->
    { grid:
      \{ minWidth } ->
        { display: "grid"
        , gridGap: "1rem"
        , gridTemplateColumns: i "repeat(auto-fit, minmax(min(" (minWidth ?|| "250px") ", 100%), 1fr))" ∷ String
        }
    }
