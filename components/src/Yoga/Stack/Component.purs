module Yoga.Stack.Component where

import Prelude
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, reactComponent)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Stack.Styles (styles)
import Yoga.Stack.Styles as Styles
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps (Styles.PropsR)

type OptionalProps r
  = ( kids ∷ Array JSX
    , className ∷ Maybe String
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS styles
  reactComponent "Stack" \props@{ kids, className } -> React.do
    { stack } <- useStyles (pick props)
    pure
      $ R.div
          { className: stack <> foldMap (" " <> _) className
          , children: kids
          }
