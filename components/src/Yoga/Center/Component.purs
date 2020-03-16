module Yoga.Center.Component where

import Prelude
import Data.Array (foldMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import Prim.Row (class Nub, class Union)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Record (disjointUnion)
import Yoga.Center.Styles as Center
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps ()

type OptionalProps r
  = ( kids ∷ Array JSX
    , className ∷ Maybe String
    | r
    )

withDefaults ∷
  ∀ r missing props.
  Union r missing props =>
  Nub props props =>
  Monoid { | missing } =>
  { | r } ->
  { | props }
withDefaults x = disjointUnion x mempty

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS Center.styles
  component "Center" \{ kids, className } -> React.do
    classes <- useStyles {}
    pure
      $ R.div
          { className: classes.centre <> foldMap (" " <> _) className
          , children: kids
          }
