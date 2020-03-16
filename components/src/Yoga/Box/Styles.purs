module Yoga.Box.Styles where

import Prelude
import Data.Maybe (Maybe, fromMaybe)
import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( padding ∷ Maybe String
    )

type Props
  = Record PropsR

styles ∷ JSSClasses YogaTheme Props ( box ∷ JSSElem Props )
styles =
  jssClasses \theme ->
    { box:
      jss
        { padding: _.padding >>> fromMaybe "var(--s1)"
        , border: "0 solid"
        , outline: theme.borderThin <> " solid transparent"
        , outlineOffset: "-" <> theme.borderThin
        }
    }
