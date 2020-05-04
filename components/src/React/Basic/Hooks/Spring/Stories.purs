module React.Basic.Hooks.Spring.Stories where

import Prelude hiding (add)
import Data.Int (pow)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import JSS (jssClasses)
import Justifill (justifill)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component, fragment, useEffect, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animatedDiv, useSpring, useTransition)
import React.Basic.Hooks.UseGesture (useDrag, withDragProps)
import Storybook.React (Storybook, add, storiesOf)
import Yoga.Box.Component as Box
import Yoga.Button.Component (mkButton)
import Yoga.Card.Component (mkCard)
import Yoga.Centre.Component as Centre
import Yoga.Cluster.Component as Cluster
import Yoga.Spec.Helpers (withSpecTheme)
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

stories ∷ Effect Storybook
stories = do
  storiesOf "Spring" do
    add "The Spring" (withSpecTheme mkAnimated)
      [ {} ]
    add "The Transition" (withSpecTheme mkTransition)
      [ {} ]
    add "The Drag" (withSpecTheme mkDragAnimated)
      [ {} ]

mkAnimated ∷ Effect (ReactComponent {})
mkAnimated = do
  button <- mkButton
  stack <- Stack.makeComponent
  centre <- Centre.makeComponent
  box <- Box.makeComponent
  cluster <- Cluster.makeComponent
  component "Animated Example" \{} -> React.do
    toggled /\ modifyToggled <- useState false
    { style, set, stop } <- useSpring $ const { opacity: 1.0, transform: "scale3d(1.0,1.0,1.0)" }
    useEffect toggled do
      set
        { opacity: if toggled then 0.7 else 1.0
        , transform: "scale3d(" <> if toggled then "0.2, 0.2, 1.0" else "1.0, 1.0, 1.0" <> ")"
        , config: { mass: 1, tension: 200, friction: 20 }
        }
      pure mempty
    pure
      $ jsx stack {}
          [ jsx cluster {}
              [ R.div_
                  [ jsx centre {}
                      [ jsx button { onClick: handler_ $ modifyToggled not } [ R.text $ if toggled then "Zoom in" else "Zoom out" ]
                      ]
                  ]
              ]
          , jsx cluster {}
              [ R.div_
                  [ jsx centre {}
                      [ animatedDiv
                          { style: css style
                          , children: [ jsx box { invert: true } [ R.text "Click the button" ] ]
                          }
                      ]
                  ]
              ]
          ]

mkTransition ∷ Effect (ReactComponent {})
mkTransition = do
  button <- mkButton
  card <- mkCard
  useStyles <-
    makeStylesJSS
      $ jssClasses \(t ∷ CSSTheme) ->
          { div:
            { marginTop: "300px", marginLeft: "300px", width: "200px", height: "200px" }
          }
  component "Transition Example" \{} -> React.do
    toggled /\ modifyToggled <- useState false
    classes <- useStyles {}
    transitions <-
      useTransition [ toggled ] Nothing
        $ css
            { from: { opacity: 0.0, transform: "translate3d(-50vw, 0px, 0.0) scale3d(0.2, 0.2, 0.2)", position: "absolute" }
            , enter: { opacity: 1.0, transform: "translate3d(0.0, 0px, 0.0) scale3d(1.0,1.0,1.0)" }
            , leave: { opacity: 0.0, transform: "translate3d(50vw, 0px, 0.0) scale3d(0.0, 0.0, 1.0)" }
            , config: { mass: 1, tension: 100, friction: 12, clamp: true, easing: pow 2 }
            , unique: true
            }
    pure
      $ fragment
      $ ( transitions
            <#> \{ item, key, props } ->
                guard (item # isJust)
                  $ if item == Just true then
                      animatedDiv
                        { className: classes.div
                        , style: props
                        , key: "you"
                        , children: [ jsx card {} [ R.text "hello, you" ] ]
                        }
                    else
                      animatedDiv
                        { className: classes.div
                        , style: props
                        , key: "again"
                        , children: [ jsx card {} [ R.text "hello, again" ] ]
                        }
        )
      <> [ jsx button { onClick: handler_ (modifyToggled not) } [ R.text "Toggle" ] ]

mkDragAnimated ∷ Effect (ReactComponent {})
mkDragAnimated = do
  card <- mkCard
  useStyles <-
    makeStylesJSS
      $ jssClasses \t ->
          { card: { width: "200px", height: "200px", marginTop: "30px" }
          }
  component "Draggable Example" \{} -> React.do
    { style, set } <- useSpring $ const { x: 0.0, y: 0.0, config: { mass: 1, tension: 210, friction: 20 } }
    classes <- useStyles {}
    bindDragProps <-
      useDrag (justifill {}) \{ down, movement: mx /\ my } ->
        set { x: if down then mx else 0.0, y: if down then my else 0.0 }
    pure
      $ animatedDiv
      $ { style: css style
        , children:
          [ jsx card { className: classes.card } [ R.text "Drag me somewhere" ]
          ]
        }
          `withDragProps`
            (bindDragProps unit)
