module React.Basic.Hooks.Spring.Stories where

import Prelude hiding (add)
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Justifill (justifill)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (component, element, fragment, useLayoutEffect, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animated, animatedDiv, useSpring, useSpringImpl)
import React.Basic.Hooks.UseGesture (useDrag, withDragProps)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, add_, addDecorator, storiesOf)
import Web.DOM.DOMTokenList (toggle)
import Yoga.Box.Component as Box
import Yoga.Spec.Helpers (withDarkTheme)
import Yoga.Stack.Component as Stack

stories ∷ Effect Storybook
stories = do
  storiesOf "Spring" do
    add "The Spring" (withDarkTheme mkAnimated)
      [ {} ]
    add "The Drag" (withDarkTheme mkDragAnimated)
      [ {} ]

mkAnimated = do
  box <- Box.makeComponent
  component "Animated Example" \{} -> React.do
    toggled /\ modifyToggled <- useState false
    springStyles /\ set <- useSpring $ const { marginTop: "80px", transform: "scale3d(" <> if toggled then "0.9, 0.9, 1.0" else "1.0, 2.0, 1.0" <> ")" }
    useLayoutEffect toggled do
      set { marginTop: "80px", transform: "scale3d(" <> if toggled then "0.9, 0.9, 1.0" else "1.0, 2.0, 1.0" <> ")" }
      pure mempty
    pure
      $ fragment
          [ R.button { children: [ R.text "Toggle" ], onClick: handler_ (modifyToggled not) }
          , animatedDiv
              { style: css (spy "toni" springStyles)
              , children: [ jsx box {} [ R.text "HIIIIIIIIIIIIOOOO" ] ]
              }
          ]

mkDragAnimated = do
  box <- Box.makeComponent
  component "Animated Example" \{} -> React.do
    toggled /\ modifyToggled <- useState false
    springStyles /\ set <- useSpring $ const { x: 0.0, y: 0.0 }
    mkDragProps <-
      useDrag \{ down, movement: mx /\ my } ->
        set { x: if down then mx else 0.0, y: if down then my else 0.0 }
    pure
      $ fragment
          [ R.button { children: [ R.text "Toggle" ], onClick: handler_ (modifyToggled not) }
          , animatedDiv
              ( { style: css springStyles
                , children: [ jsx box {} [ R.text "HIIIIIIIIIIIIOOOO" ] ]
                }
                  `withDragProps`
                    mkDragProps
              )
          ]
