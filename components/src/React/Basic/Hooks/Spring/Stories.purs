module React.Basic.Hooks.Spring.Stories where

import Prelude hiding (add)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import JSS (jssClasses)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component, fragment, useLayoutEffect, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animatedDiv, useSpring, useTransition)
import React.Basic.Hooks.UseGesture (useDrag, withDragProps)
import Storybook.React (Storybook, add, storiesOf)
import Yoga.Box.Component as Box
import Yoga.Button.Component (mkButton)
import Yoga.Spec.Helpers (withDarkTheme)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

stories ∷ Effect Storybook
stories = do
  storiesOf "Spring" do
    add "The Spring" (withDarkTheme mkAnimated)
      [ {} ]
    add "The Transition" (withDarkTheme mkTransition)
      [ {} ]
    add "The Drag" (withDarkTheme mkDragAnimated)
      [ {} ]

mkAnimated ∷ Effect (ReactComponent {})
mkAnimated = do
  box <- Box.makeComponent
  component "Animated Example" \{} -> React.do
    toggled /\ modifyToggled <- useState false
    { style, set, stop } <- useSpring $ const { opacity: 0.7, transform: "scale3d(0.9,0.9,0.9)" }
    let
      _ =
        unsafePerformEffect
          $ set { opacity: if toggled then 0.7 else 0.9, transform: "scale3d(" <> if toggled then "0.9, 0.9, 1.0" else "0.95, 0.95, 1.0" <> ")" }
    pure
      $ fragment
          [ R.button { children: [ R.text "Toggle" ], onClick: handler_ $ modifyToggled not }
          , animatedDiv
              { style: css style
              , children: [ jsx box {} [ R.text "Click the button" ] ]
              }
          ]

mkTransition ∷ Effect (ReactComponent {})
mkTransition = do
  button <- mkButton
  useStyles <-
    makeStylesJSS
      $ jssClasses \(t ∷ CSSTheme) ->
          { div:
            { marginTop: "300px", marginLeft: "300px", width: "200px", height: "200px" }
          , blue: { background: t.textColour }
          , red: { background: t.white }
          }
  component "Transition Example" \{} -> React.do
    toggled /\ modifyToggled <- useState false
    classes <- useStyles {}
    transitions <-
      useTransition [ toggled ] Nothing
        $ css
            { from: { opacity: 0.0, transform: "translate3d(-1000px, 0px, 0.0)", position: "absolute" }
            , enter: { opacity: 1.0, transform: "translate3d(0.0, 0px, 0.0)" }
            , leave: { opacity: 0.0, transform: "scale3d(0, 0, 1)" }
            }
    pure
      $ fragment
      $ ( transitions
            <#> \{ item, key, props } ->
                if item == Just true then
                  animatedDiv
                    { className: classes.div <> " " <> classes.red
                    , style: props
                    , children: [ R.text "hello, you" ]
                    }
                else
                  animatedDiv
                    { className: classes.div <> " " <> classes.blue
                    , style: props
                    , children: [ R.text "I am different" ]
                    }
        )
      <> [ jsx button { onClick: handler_ (modifyToggled not) } [ R.text "Toggle" ] ]

mkDragAnimated ∷ Effect (ReactComponent {})
mkDragAnimated = do
  box <- Box.makeComponent
  useStyles <- makeStylesJSS $ jssClasses \t -> { div: { width: "200px", height: "200px", background: "hotpink" } }
  component "Draggable Example" \{} -> React.do
    { style, set } <- useSpring $ const { x: 0.0, y: 0.0 }
    classes <- useStyles {}
    mkDragProps <-
      useDrag \{ down, movement: mx /\ my } ->
        set { x: if down then mx else 0.0, y: if down then my else 0.0 }
    pure
      $ fragment
          [ animatedDiv
              ( { style: css style
                , className: classes.div
                , children: [ jsx box {} [ R.text "Drag me somewhere" ] ]
                }
                  `withDragProps`
                    mkDragProps
              )
          ]
