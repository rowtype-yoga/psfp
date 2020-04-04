module Yoga.Imposter.Component where

import Prelude
import Data.Array (foldMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (CSS)
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Helpers (orUndefined)
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animatedDiv)
import Record.Extra (pick)
import Yoga.Helpers (ifJustFalse, (?||))
import Yoga.Imposter.Styles as Style
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps Style.PropsR

type OptionalProps r
  = ( kids ∷ Array JSX
    , className ∷ Maybe String
    , onClick ∷ Maybe EventHandler
    , style ∷ Maybe CSS
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS Style.styles
  component "Imposter" \props@{ kids, className } -> React.do
    classes <- useStyles (pick props)
    pure
      $ animatedDiv
          { className: classes.imposter <> ifJustFalse props.breakout (" " <> classes.contain) <> foldMap (" " <> _) className
          , onClick: props.onClick ?|| (handler_ (pure unit))
          , children: kids
          , style: props.style # orUndefined
          }
