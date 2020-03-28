module Yoga.Modal.Component where

import Prelude
import CSS (JustifyContentValue(..), spaceBetween)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign.Object as Obj
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.DOM.Events (stopPropagation)
import React.Basic.DOM.SVG as SVG
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Box.Component as Box
import Yoga.Cluster.Component as Cluster
import Yoga.Helpers ((?||))
import Yoga.Imposter.Component as Imposter
import Yoga.Modal.Styles as Style
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = ( title ∷ String
    , content ∷ JSX
    , onClose ∷ Maybe (Effect Unit)
    | Style.PropsR
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  imposter <- Imposter.makeComponent
  box <- Box.makeComponent
  stack <- Stack.makeComponent
  cluster <- Cluster.makeComponent
  useStyles <- makeStylesJSS Style.styles
  component "Modal" \props -> React.do
    cs <- useStyles (pick props)
    let
      darkOverlay =
        jsx imposter
          { className: cs.darkOverlay <> " " <> cs.fadeIn
          , fixed: true
          , breakout: false
          , onClick: props.onClose ?|| mempty # handler_
          }

      dialogImposter =
        jsx imposter
          { className: cs.dialog
          , onClick: handler stopPropagation mempty
          , fixed: true
          }

      dialogBox =
        jsx box
          { className: cs.box <> " " <> cs.zoomIn
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
              , children:
                [ props.onClose # foldMap (closeIcon cs <<< handler_)
                ]
              }
          ]
    pure
      $ darkOverlay
          [ dialogImposter
              [ dialogBox
                  [ dialogBoxStack
                      [ titleCluster [ title ]
                      , props.content
                      ]
                  ]
              ]
          ]

closeIcon ∷ ∀ a. { closeIcon ∷ String | a } -> EventHandler -> JSX
closeIcon classes onClick =
  SVG.svg
    { xmlns: "http://www.w3.org/2000/svg"
    , viewBox: "5 0 100 105"
    , fillRule: "evenodd"
    , clipRule: "evenodd"
    , strokeLinejoin: "round"
    , strokeMiterlimit: "2"
    , _data: Obj.singleton "testid" "close-icon-svg"
    , onClick
    , className: classes.closeIcon
    , children:
      [ SVG.g_
          [ SVG.path
              { d: "M101.703,36.06c2.054,-2.054 2.054,-5.388 0,-7.442l-7.441,-7.441c-2.054,-2.054 -5.388,-2.054 -7.442,0l-65.643,65.643c-2.054,2.054 -2.054,5.388 0,7.442l7.441,7.441c2.054,2.054 5.388,2.054 7.442,0l65.643,-65.643Z"
              }
          , SVG.path
              { d:
                "M86.82,101.703c2.054,2.054 5.388,2.054 7.442,0l7.441,-7.441c2.054,-2.054 2.054,-5.388 0,-7.442l-65.643,-65.643c-2.054,-2.054 -5.388,-2.054 -7.442,0l-7.441,7.441c-2.054,2.054 -2.054,5.388 0,7.442l65.643,65.643Z"
              }
          ]
      ]
    }
