module Yoga.Cover.Component where

import Prelude
import Data.Foldable (foldMap)
import Data.Array as Array
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, reactComponent)
import React.Basic.Hooks as React
import Yoga.Cover.Styles as Cover
import Yoga.Theme.Styles (makeStylesJSS)

type Props =
  Record PropsR

type PropsR =
  OptionalProps ()

type OptionalProps r =
  ( header ∷ Maybe JSX
  , footer ∷ Maybe JSX
  , kids ∷ Array JSX
  , className ∷ Maybe String
  | r
  )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS Cover.styles
  reactComponent "Cover" \{ header, footer, kids, className } -> React.do
    classes <- useStyles {}
    pure
      $ R.div
          { className: classes.cover <> foldMap (" " <> _) className
          , children:
            [ R.div { className: classes.header, children: Array.fromFoldable header }
            , R.div { className: classes.principal, children: kids }
            , R.div { className: classes.footer, children: Array.fromFoldable footer }
            ]
          }
