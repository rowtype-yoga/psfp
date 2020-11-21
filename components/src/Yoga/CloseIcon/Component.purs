module Yoga.CloseIcon.Component where

import Prelude
import Data.Int (round, toNumber)
import Data.Interpolate (i)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign.Object as Obj
import Math (pi, pow, sqrt)
import React.Basic.DOM (CSS)
import React.Basic.DOM.SVG as R
import React.Basic.DOM.SVG as SVG
import React.Basic.Events (handler_)
import React.Basic.Helpers (orUndefined)
import React.Basic.Hooks (ReactComponent, reactComponent, useState)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Box.Component as Box
import Yoga.CloseIcon.Styles as Style
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = ( onClick ∷ Effect Unit
    , style ∷ Maybe CSS
    | Style.PropsR
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  box <- Box.makeComponent
  useStyles <- makeStylesJSS Style.styles
  reactComponent "CloseIcon" \(props ∷ Props) -> React.do
    cs <- useStyles (pick props)
    animationDone /\ modifyAnimationDone <- useState false
    pure
      $ SVG.svg
          { viewBox: i "0 0 " width " " width
          , _data: Obj.singleton "testid" "close-icon-svg"
          , onClick: handler_ props.onClick
          , style: props.style # orUndefined
          , fill: "none"
          , className: cs.closeIcon
          , children:
            [ R.path
                { d: i "M" padding "," padding "L" (width - padding) "," (width - padding)
                }
            , R.path
                { d: i "M" padding "," (width - padding) ",L" (width - padding) "," padding
                }
            ]
          }
  where
  padding = 45
  width = 200
  cx = width / 2
  r = round ((toNumber width * 0.8) / 2.0)
  circleCircumf = round $ 2.0 * pi * toNumber r
  aSquare = toNumber (width - (2 * padding)) `pow` 2.0
  lineLen = sqrt (aSquare * 2.0)
