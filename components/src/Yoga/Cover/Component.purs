module Yoga.Cover.Component where

import Prelude
import Data.Array (foldMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import Yoga.Cover.Styles as Cover
import Prim.Row (class Nub, class Union)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Record (disjointUnion)
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps ()

type OptionalProps r
  = ( header ∷ JSX
    , footer ∷ JSX
    , principal ∷ JSX
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
  useStyles <- makeStylesJSS Cover.styles
  component "Cover" \{ header, footer, principal, className } -> React.do
    classes <- useStyles {}
    pure
      $ R.div
          { className: classes.cover <> foldMap (" " <> _) className
          , children:
            [ R.div { className: classes.header, children: pure header }
            , R.div { className: classes.principal, children: pure principal }
            , R.div { className: classes.footer, children: pure footer }
            ]
          }
