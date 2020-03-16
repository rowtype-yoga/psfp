module Yoga.Stack.Styles where

import Prelude
import Data.Foldable (foldMap)
import Data.Maybe (Maybe, fromMaybe)
import Foreign.Object as Object
import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( space ∷ Maybe String
    , splitAfter ∷ Maybe Int
    )

type Props
  = Record PropsR

styles ∷
  JSSClasses YogaTheme Props
    ( stack ∷ JSSElem Props
    )
styles =
  jssClasses \theme ->
    { stack:
      jss
        $ \(props ∷ Props) ->
            jss
              { "--space": props.space # fromMaybe "1.5rem"
              , display: "flex"
              , flexDirection: "column"
              , justifyContent: "flex-start"
              , "& > *":
                { marginTop: 0
                , marginBottom: 0
                }
              , "& > * + *":
                { marginTop: "var(--space)"
                }
              }
              <> splitAfter props
    }

splitAfter ∷ Props -> JSSElem Props
splitAfter props =
  jss
    $ props.splitAfter
    # foldMap (\n -> nthChild n <> onlyChild)
  where
  nthChildKey n = "& > :nth-child(" <> show n <> ")"
  onlyChild = Object.singleton "&:only-child" (jss { height: "100%" })
  nthChild n = Object.singleton (nthChildKey n) (jss { marginBottom: "auto" })
