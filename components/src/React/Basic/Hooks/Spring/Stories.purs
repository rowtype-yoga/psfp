module React.Basic.Hooks.Spring.Stories where

import Prelude hiding (add)
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Effect (Effect)
import JSS (jssClasses)
import Justifill (justifill)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component, element, fragment, useLayoutEffect, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animated, animatedDiv, useSpring, useSpringImpl)
import React.Basic.Hooks.UseGesture (useDrag, withDragProps)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, add_, addDecorator, storiesOf)
import Web.DOM.DOMTokenList (toggle)
import Yoga.Box.Component as Box
import Yoga.Spec.Helpers (withDarkTheme)
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (makeStylesJSS)

stories ∷ Effect Storybook
stories = do
  storiesOf "Spring" do
    add "The Spring" (withDarkTheme mkAnimated)
      [ {} ]
    add "The Drag" (withDarkTheme mkDragAnimated)
      [ {} ]

mkAnimated ∷ Effect (ReactComponent {})
mkAnimated = do
  box <- Box.makeComponent
  component "Animated Example" \{} -> React.do
    toggled /\ modifyToggled <- useState false
    { style, set, stop } <- useSpring $ const { opacity: 0.7, transform: "scale3d(" <> if toggled then "0.9, 0.9, 1.0" else "1.0, 2.0, 1.0" <> ")" }
    useLayoutEffect toggled do
      (spy "fuck" set) { opacity: if toggled then 0.7 else 0.9, transform: "scale3d(" <> if toggled then "0.9, 0.9, 1.0" else "1.0, 2.0, 1.0" <> ")" }
      pure mempty
    pure
      $ fragment
          [ animatedDiv
              { style: css style
              , children: [ jsx box {} [ R.text "Click the button" ] ]
              }
          , R.button { children: [ R.text "Toggle" ], onClick: handler_ (modifyToggled not) }
          ]

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
