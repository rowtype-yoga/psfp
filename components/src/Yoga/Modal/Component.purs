module Yoga.Modal.Component where

import Prelude
import CSS (JustifyContentValue(..), spaceBetween)
import Effect (Effect)
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
import Yoga.Imposter.Component as Imposter
import Yoga.Modal.Styles as Style
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = ( title ∷ String
    , content ∷ JSX
    , onClose ∷ Effect Unit
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
          { className: cs.darkOverlay <> " animated fadeIn"
          , fixed: true
          , breakout: false
          , onClick: handler_ props.onClose
          }

      dialogImposter =
        jsx imposter
          { className: cs.dialog ∷ String
          , onClick: handler stopPropagation (\_ -> pure unit)
          , fixed: true
          }

      dialogBox =
        jsx box
          { className: cs.box <> " animated lightSpeedIn"
          , invert: true
          } -- [TODO]: reconsider

      dialogBoxStack =
        jsx stack
          { className: cs.dialogBoxStack ∷ String
          }

      titleCluster =
        jsx cluster
          { className: cs.titleCluster ∷ String
          , justify: JustifyContentValue spaceBetween
          }

      title =
        R.div_
          [ R.h4 { className: cs.title, children: [ R.text props.title ] }
          , R.div
              { className: cs.closeIcon
              , children:
                [ closeIcon (handler_ props.onClose) cs
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

closeIcon ∷ ∀ a. EventHandler -> { closeIcon ∷ String | a } -> JSX
closeIcon onClick classes =
  SVG.svg
    { xmlns: "http://www.w3.org/2000/svg"
    , viewBox: "19 19 85 85"
    , fillRule: "evenodd"
    , clipRule: "evenodd"
    , strokeLinejoin: "round"
    , strokeMiterlimit: "2"
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
