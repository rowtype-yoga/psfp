module React.Basic.Hooks.Spring.Stories where

import Prelude hiding (add)
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (component, element, fragment, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animated, animatedDiv, useSpring, useSpringImpl)
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

mkAnimated = do
  box <- Box.makeComponent
  component "Animated Example" \{} -> React.do
    toggled /\ modifyToggled <- useState false
    springStyles <- useSpring (css { marginTop: "200px", transform: "scale3d(" <> if toggled then "0.2, 0.2, 1.0" else "10.0, 10.0, 0.0" <> ")" })
    pure
      $ fragment
          [ R.button { children: [ R.text "Toggle" ], onClick: handler_ (modifyToggled not) }
          , animatedDiv
              { style: springStyles
              , children: [ jsx box {} [ R.text "HIIIIIIIIIIIIOOOO" ] ]
              }
          ]
