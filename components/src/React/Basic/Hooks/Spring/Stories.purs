module React.Basic.Hooks.Spring.Stories where

import Prelude hiding (add)

import CSS (backgroundColor, height, position, relative, vh, vw, width)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Int (pow)
import Data.Time.Duration (Seconds(..), convertDuration)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import JSS (jssClasses)
import Justifill (justifill)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, reactComponent, useEffect, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import React.Basic.Hooks.Spring (animatedDiv, useSpring, useTransition)
import React.Basic.Hooks.UseGesture (useDrag, withDragProps)
import Storybook.React (Stories, add, storiesOf)
import Yoga.Box.Component as Box
import Yoga.Button.Component (ButtonType(..), mkButton)
import Yoga.Card.Component (mkCard)
import Yoga.Centre.Component as Centre
import Yoga.Cluster.Component as Cluster
import Yoga.Imposter.Component as Imposter
import Yoga.Spec.Helpers (withSpecTheme)
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

stories ∷ Stories
stories = do
  storiesOf "Spring" do
    add "The Spring" (withSpecTheme mkAnimated)
      [ {} ]
    add "The Transition" (withSpecTheme mkTransition)
      [ {} ]
    add "The Countdown Transition" (withSpecTheme mkCountdown)
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
  reactComponent "Animated Example" \{} -> React.do
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
  imposter <- Imposter.makeComponent
  box <- Box.makeComponent
  useStyles <-
    makeStylesJSS
      $ jssClasses \(t ∷ CSSTheme) ->
          { div:
            { marginTop: "300px", marginLeft: "300px", width: "200px", height: "200px" }
          , blueCard:
            do
              backgroundColor t.blue
              position relative
          , greenCard:
            do
              backgroundColor t.green
              position relative
          }
  reactComponent "Transition Example" \{} -> React.do
    toggled /\ modifyToggled <- useState false
    classes <- useStyles {}
    transitionFn <-
      useTransition [ toggled ]
        { from: { opacity: 0.0, transform: "scale3d(4.2, 4.2, 4.2)", position: "absolute" }
        , enter: { opacity: 1.0, transform: "scale3d(1.0,1.0,1.0)" }
        , leave: { opacity: 0.0, transform: "scale3d(0.0, 0.0, 1.0)" }
        , config: { mass: 1, tension: 100, friction: 12, clamp: true, easing: pow 2 }
        , unique: true
        }
    let
      transitionsFragment = transitionFn f
        where
        f props item { phase, key } index = go
          where
          go = case item of
            true ->
              animatedDiv
                { style: css props
                , key
                , children:
                  pure $ jsx card { className: classes.blueCard } [ jsx box {} [ R.text "First" ] ]
                }
            false ->
              animatedDiv
                { style: css props
                , key
                , children: pure $ jsx card { className: classes.greenCard } [ jsx box {} [ R.text "Second" ] ]
                }
    pure
      $ jsx imposter {} [ transitionsFragment ]
      <> jsx button { onClick: handler_ (modifyToggled not) } [ R.text "Toggle" ]

data Countdown
  = CountdownNotStarted
  | CountdownRunning Int
  | CountdownFinished

derive instance genericCountdown ∷ Generic Countdown _
instance eqCountdown ∷ Eq Countdown where
  eq = genericEq

mkCountdown ∷ Effect (ReactComponent {})
mkCountdown = do
  button <- mkButton
  card <- mkCard
  imposter <- Imposter.makeComponent
  box <- Box.makeComponent
  useStyles <-
    makeStylesJSS
      $ jssClasses \(t ∷ CSSTheme) ->
          { div:
            { marginTop: "300px", marginLeft: "300px", width: "200px", height: "200px" }
          , blueCard:
            do
              backgroundColor t.blue
              position relative
              width (50.0 # vw)
              height (50.0 # vh)
          }
  reactComponent "Transition Countdown Example" \{} -> React.do
    countdown /\ modifyCountdown <- useState CountdownNotStarted
    classes <- useStyles {}
    transitionFn <-
      useTransition [ countdown ]
        { from: { opacity: 0.0, transform: "scale3d(4.2, 4.2, 4.2)", position: "absolute" }
        , enter: { opacity: 1.0, transform: "scale3d(1.0,1.0,1.0)" }
        , leave: { opacity: 0.0, transform: "scale3d(0.0, 0.0, 1.0)" }
        , config: { mass: 1, tension: 100, friction: 12, clamp: true, easing: pow 2 }
        , unique: true
        }
    useAff countdown do
      delay (1.0 # Seconds # convertDuration)
      liftEffect
        $ modifyCountdown case _ of
            CountdownNotStarted -> CountdownNotStarted
            CountdownFinished -> CountdownFinished
            CountdownRunning 1 -> CountdownFinished
            CountdownRunning n -> CountdownRunning (n - 1)
    let
      transitionsFragment = transitionFn f
        where
        f props item { phase, key } index = go
          where
          go = case item of
            CountdownNotStarted -> mempty
            CountdownFinished ->
              animatedDiv
                { style: css props
                , key: "finished"
                , children:
                  [ jsx
                      imposter
                      {}
                      [ R.div
                          { onClick: handler_ $ modifyCountdown (const CountdownNotStarted)
                          , children:
                            [ jsx card { className: classes.blueCard }
                                [ jsx box {} [ R.h1_ [ R.text "Hooray!" ] ] ]
                            ]
                          }
                      ]
                  ]
                }
            CountdownRunning n ->
              animatedDiv
                { style: css props
                , key: "running-" <> show n
                , children:
                  pure $ jsx imposter {} [ R.h2_ [ R.text $ show n ] ]
                }
    pure
      $ jsx imposter {} [ transitionsFragment ]
      <> jsx button { buttonType: if countdown /= CountdownNotStarted then DisabledButton else HighlightedButton, onClick: handler_ (modifyCountdown (const $ CountdownRunning 3)) } [ R.text "Start Countdown" ]

mkDragAnimated ∷ Effect (ReactComponent {})
mkDragAnimated = do
  card <- mkCard
  useStyles <-
    makeStylesJSS
      $ jssClasses \t ->
          { card: { width: "200px", height: "200px", marginTop: "30px" }
          }
  reactComponent "Draggable Example" \{} -> React.do
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
