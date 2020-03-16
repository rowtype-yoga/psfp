module Yoga.WithSidebar.Styles where

import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ()

type Props
  = Record PropsR

styles ∷
  JSSClasses YogaTheme Props
    ( notSidebar ∷ JSSElem Props
    , sidebar ∷ JSSElem Props
    , withSidebar ∷ JSSElem Props
    )
styles =
  jssClasses \theme ->
    { withSidebar:
      jss
        { display: "flex"
        , flexWrap: "wrap"
        }
    , sidebar: jss { flexBasis: "20rem", flexGrow: 1 }
    , notSidebar: jss { flexBasis: 0, flexGrow: 999, minWidth: "50%" }
    }
