module Yoga.Panel.Component where

import Prelude
import Data.Array (foldMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import Yoga.Panel.Styles as Panel
import Prim.Row (class Nub, class Union)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Record (disjointUnion)
import Yoga.Theme.Styles (makeStyles)

type Props
  = Record PropsR

type PropsR
  = OptionalProps ()

type OptionalProps r
  = ( kids :: Array JSX
    , className :: Maybe String
    | r
    )

withDefaults ∷
  forall r missing props.
  Union r missing props =>
  Nub props props =>
  Monoid { | missing } =>
  { | r } ->
  { | props }
withDefaults x = disjointUnion x mempty

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStyles Panel.styles
  component "Panel" \{ kids, className } -> React.do
    classes <- useStyles
    pure
      $ R.div
          { className: classes.panel <> foldMap (" " <> _) className
          , children: kids
          }
