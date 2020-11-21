module Yoga.ClickAway.Stories where

import Prelude hiding (add)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
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
import Yoga.ClickAway.Component as ClickAway

stories ∷ _ -> Effect Storybook
stories = do
  storiesOf "ClickAway" do
    addDecorator fullScreenDecorator
    add "The ClickAway" ClickAway.makeComponent
      [ ( justifill
            { onClick: mempty ∷ Effect Unit
            }
        )
      ]
    add "Interactive ClickAway Example" mkExample
      [ {}
      ]

mkExample ∷ Effect (ReactComponent {})
mkExample = do
  clickaway <- ClickAway.makeComponent
  centre <- Centre.makeComponent
  button <- mkButton
  component "ClickAwayStory" \{} -> React.do
    state /\ modState <- useState { open: true }
    pure
      $ fragment
          [ jsx centre {}
              [ jsx button
                  { onClick: handler_ (modState (_ { open = true }))
                  }
                  [ R.text "Open ClickAway" ]
              ]
          , R.p_ [ R.text "Here's some text to see what happens when there's actual content" ]
          , guard (state.open)
              $ element clickaway
                  { onClick: modState (_ { open = false })
                  , allowEscape: Just true
                  , style: Nothing
                  }
          ]
