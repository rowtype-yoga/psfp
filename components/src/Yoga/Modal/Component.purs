module Yoga.Modal.Component where

import Prelude
import CSS (JustifyContentValue(..), spaceBetween)
import Data.Array (fromFoldable)
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (CSS)
import React.Basic.DOM as R
import React.Basic.DOM.Events (stopPropagation)
import React.Basic.Events (handler)
import React.Basic.Helpers (jsx, orUndefined)
import React.Basic.Hooks (ReactComponent, reactComponent)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Box.Component as Box
import Yoga.Cluster.Component as Cluster
import Yoga.Imposter.Component as Imposter
import Yoga.Modal.Styles as Style
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = ( title ∷ String
    , kids ∷ Array JSX
    , icon ∷ Maybe JSX
    , style ∷ Maybe CSS
    | Style.PropsR
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  imposter <- Imposter.makeComponent
  box <- Box.makeComponent
  stack <- Stack.makeComponent
  cluster <- Cluster.makeComponent
  useStyles <- makeStylesJSS Style.styles
  reactComponent "Modal" \props -> React.do
    cs <- useStyles (pick props)
    let
      dialogImposter =
        jsx imposter
          { className: cs.dialog
          , onClick: handler stopPropagation mempty
          , fixed: true
          , style: props.style # orUndefined
          }

      dialogBox =
        jsx box
          { className: cs.box
          }

      dialogBoxStack =
        jsx stack
          { className: cs.dialogBoxStack
          }

      titleCluster =
        jsx cluster
          { className: cs.titleCluster
          , justify: JustifyContentValue spaceBetween
          , space: "0"
          }

      title =
        R.div_
          [ R.h4 { className: cs.title, children: [ R.text props.title ] }
          , R.div
              { className: cs.closeIcon
              , children: fromFoldable props.icon
              }
          ]
    pure
      $ dialogImposter
          [ dialogBox
              [ dialogBoxStack
                  $ [ titleCluster [ title ]
                    ]
                  <> props.kids
              ]
          ]
