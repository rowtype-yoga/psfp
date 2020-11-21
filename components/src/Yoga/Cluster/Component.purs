module Yoga.Cluster.Component where

import Prelude

import Data.Array (foldMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, reactComponent)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Cluster.Styles as Style
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps Style.PropsR

type OptionalProps r
  = ( kids ∷ Array JSX
    , className ∷ Maybe String
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS Style.styles
  reactComponent "Cluster" \props@{ kids, className } -> React.do
    classes <- useStyles (pick props)
    pure
      $ R.div
          { className: classes.cluster <> foldMap (" " <> _) className
          , children: kids
          }
