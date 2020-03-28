module Yoga.Modal.Stories where

import Prelude hiding (add)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Justifill (justifill)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component, element, fragment, useState)
import React.Basic.Hooks as React
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Button.Component (mkButton)
import Yoga.Centre.Component as Centre
import Yoga.Modal.Component as Modal

stories ∷ Effect Storybook
stories = do
  storiesOf "Modal" do
    addDecorator fullScreenDecorator
    add "The Modal" Modal.makeComponent
      [ justifill
          { content: R.text "I'm a Modal"
          , title: "Something important has happened this is excessively, if not prohibitively long title text to see what will happen"
          }
      ]
    add "Interactive Modal Example" mkExample
      [ {}
      ]

mkExample ∷ Effect (ReactComponent {})
mkExample = do
  modal <- Modal.makeComponent
  centre <- Centre.makeComponent
  button <- mkButton
  component "ModalStory" \{} -> React.do
    state /\ modState <- useState { open: true }
    pure
      $ fragment
          [ jsx centre {}
              [ jsx button
                  { onClick: handler_ (modState (_ { open = true }))
                  }
                  [ R.text "Open Modal" ]
              ]
          , R.p_ [ R.text "Here's some text to see what happens when there's actual content" ]
          , guard (state.open)
              $ element modal
                  { title: "Warning"
                  , content:
                    R.text "This is more interactive"
                  , onClose: Just $ modState (_ { open = false })
                  }
          ]
