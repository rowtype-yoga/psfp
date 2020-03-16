module Yoga.Box.Component where

import Prelude
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Yoga.Box.Styles (styles)
import Yoga.Box.Styles as Styles
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps (Styles.PropsR)

type OptionalProps r
  = ( kids :: Array JSX
    , className :: Maybe String
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS styles
  component "Box" \{ kids, className, padding } -> React.do
    classes <- useStyles { padding }
    pure
      $ R.div
          { className: classes.box <> foldMap (" " <> _) className
          , children: kids
          }
