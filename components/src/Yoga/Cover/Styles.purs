module Yoga.Cover.Styles where

import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ()

type Props
  = Record PropsR

styles ∷
  JSSClasses YogaTheme Props
    ( cover ∷ JSSElem Props
    , footer ∷ JSSElem Props
    , header ∷ JSSElem Props
    , principal ∷ JSSElem Props
    )
styles =
  jssClasses \theme ->
    { cover:
      jss
        { padding: "1rem"
        , minHeight: "100vh"
        , display: "flex"
        , flexDirection: "column"
        }
    , header:
      jss
        { marginTop: 0
        , marginBottom: "1rem"
        }
    , principal:
      jss
        { marginTop: "auto"
        , marginBottom: "auto"
        }
    , footer:
      jss
        { marginTop: "1rem"
        , marginBottom: 0
        }
    }
