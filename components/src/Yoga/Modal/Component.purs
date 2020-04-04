module Yoga.Modal.Component where

import Prelude
import CSS (JustifyContentValue(..), spaceBetween)
import Data.Foldable (foldMap)
import Data.Int (round, toNumber)
import Data.Interpolate (i)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign.Object as Obj
import Math (pi, pow, sqrt)
import React.Basic (JSX, fragment)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (stopPropagation)
import React.Basic.DOM.SVG as SVG
import React.Basic.Events (handler, handler_)
import React.Basic.Extra.Hooks.UseKeyUp (useKeyUp)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component, element, useEffect, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animatedPath, useSpring)
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
  closeIcon <- mkCloseIcon
  useStyles <- makeStylesJSS Style.styles
  component "Modal" \props -> React.do
    cs <- useStyles (pick props)
    useKeyUp 27 $ props.onClose ?|| pure unit
    animationDone /\ modifyAnimationDone <- useState false
    overlayStyle <- useSpring $ const { opacity: 0.0 }
    dialogStyle <-
      useSpring
        $ const
            { transform: "translate3d(-50%, -50%, 0) scale3d(0.2, 0.2, 0.0)"
            , config: { mass: 0.8, tension: 120, friction: 14 }
            }
    useEffect unit do
      overlayStyle.set { opacity: 1.0 }
      dialogStyle.set
        { transform: "translate3d(-50%, -50%, 0) scale3d(1.0, 1.0, 1.0)"
        , onRest: modifyAnimationDone (const true)
        }
      pure mempty
    let
      darkOverlay =
        jsx imposter
          { className: cs.darkOverlay
          , fixed: true
          , breakout: false
          , onClick: props.onClose ?|| mempty # handler_
          , style: css overlayStyle.style
          }
          []

      dialogImposter =
        jsx imposter
          { className: cs.dialog
          , onClick: handler stopPropagation mempty
          , fixed: true
          , style: css dialogStyle.style
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
              , children:
                [ props.onClose
                    # foldMap
                        ( \onClick ->
                            ( element closeIcon
                                { className: cs.closeIcon, onClick, startAnimation: animationDone }
                            )
                        )
                ]
              }
          ]
    pure
      $ fragment
          [ darkOverlay
          , dialogImposter
              [ dialogBox
                  [ dialogBoxStack
                      [ titleCluster [ title ]
                      , props.content
                      ]
                  ]
              ]
          ]

mkCloseIcon ∷
  Effect
    ( ReactComponent
        { className ∷ String
        , onClick ∷ Effect Unit
        , startAnimation ∷ Boolean
        }
    )
mkCloseIcon = do
  box <- Box.makeComponent
  component "CloseIcon" \{ className, onClick, startAnimation } -> React.do
    { style, set, stop } <-
      useSpring
        $ const
            { strokeDasharray: "0, " <> show lineLen
            }
    circleStyle <-
      useSpring
        $ const
            { strokeDasharray: "0," <> show circleCircumf
            , onRest: set { "strokeDasharray": i lineLen "," lineLen ∷ String }
            }
    useEffect startAnimation do
      when startAnimation $ circleStyle.set { "strokeDasharray": (i circleCircumf "," circleCircumf) ∷ String }
      pure mempty
    pure
      $ SVG.svg
          { viewBox: i "0 0 " width " " width
          , _data: Obj.singleton "testid" "close-icon-svg"
          , onClick: handler_ onClick
          , fill: "none"
          , className
          , children:
            [ animatedPath
                { d: i "M" padding "," padding "L" (width - padding) "," (width - padding)
                , style: css style
                }
            , animatedPath
                { d: i "M" padding "," (width - padding) ",L" (width - padding) "," padding
                , style: css style
                }
            , animatedPath
                { d: move <> line1 <> line2
                , style: css circleStyle.style
                }
            ]
          }
  where
  move = i "M " (cx - r) "," cx
  line1 = i " a " r "," r " 0 1,0 " (r * 2) ",0 "
  line2 = i " a " r "," r " 0 1,0 " (-(r * 2)) ",0 "
  padding = 70
  width = 200
  cx = width / 2
  r = round ((toNumber width * 0.8) / 2.0)
  circleCircumf = round $ 2.0 * pi * toNumber r
  aSquare = toNumber (width - (2 * padding)) `pow` 2.0
  lineLen = sqrt (aSquare * 2.0)
