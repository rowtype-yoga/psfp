module Yoga.WithSidebar.Component where

import Prelude
import Data.Array (foldMap)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.WithSidebar.Styles as WithSidebar

type Props
  = Record PropsR

type PropsR
  = OptionalProps
      ( sidebarChildren ∷ Array JSX
      , notSidebarChildren ∷ Array JSX
      )

type OptionalProps r
  = ( className ∷ Maybe String
    , sidebarClassName ∷ Maybe String
    , notSidebarClassName ∷ Maybe String
    , sidebarRight ∷ Maybe Boolean
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS WithSidebar.styles
  component "WithSidebar" \{ sidebarChildren
  , notSidebarChildren
  , className
  , sidebarClassName
  , notSidebarClassName
  , sidebarRight
  } -> React.do
    classes <- useStyles {}
    pure
      $ R.div
          { className: classes.withSidebar <> foldMap (" " <> _) className
          , children:
            (if sidebarRight == (Just true) then A.reverse else identity)
              [ R.div { className: classes.sidebar <> foldMap (" " <> _) sidebarClassName, children: sidebarChildren }
              , R.div { className: classes.notSidebar <> foldMap (" " <> _) notSidebarClassName, children: notSidebarChildren }
              ]
          }
