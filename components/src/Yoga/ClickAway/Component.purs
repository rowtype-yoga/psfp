module Yoga.ClickAway.Component where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import React.Basic.DOM (CSS)
import React.Basic.Events (handler_)
import React.Basic.Extra.Hooks.UseKeyUp (KeyCode(..), useKeyUp)
import React.Basic.Helpers (jsx, orUndefined)
import React.Basic.Hooks (ReactComponent, reactComponent, useState)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.ClickAway.Styles as Style
import Yoga.Helpers (ifJustTrue)
import Yoga.Imposter.Component as Imposter
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = ( onClick ∷ Effect Unit
    , allowEscape ∷ Maybe Boolean
    , style ∷ Maybe CSS
    | Style.PropsR
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  imposter <- Imposter.makeComponent
  useStyles <- makeStylesJSS Style.styles
  reactComponent "ClickAway" \props -> React.do
    cs <- useStyles (pick props)
    useKeyUp Escape $ ifJustTrue props.allowEscape props.onClick
    animationDone /\ modifyAnimationDone <- useState false
    pure
      $ jsx imposter
          { className: cs.darkOverlay
          , fixed: true
          , breakout: false
          , onClick: Just (handler_ props.onClick)
          , style: props.style # orUndefined
          }
          []
