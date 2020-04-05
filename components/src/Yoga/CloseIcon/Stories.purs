module Yoga.CloseIcon.Stories where

import Prelude hiding (add)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component, element, fragment, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (useTransition)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Button.Component (mkButton)
import Yoga.Centre.Component as Centre
import Yoga.CloseIcon.Component as CloseIcon

stories ∷ Effect Storybook
stories = do
  storiesOf "CloseIcon" do
    addDecorator fullScreenDecorator
    add "The CloseIcon" CloseIcon.makeComponent
      [ justifill
          { onClick: pure unit ∷ Effect Unit
          }
      ]
    add "Interactive CloseIcon Example" mkExample
      [ {}
      ]
    add "Interactive Animated CloseIcon Example" mkAnimatedExample
      [ {}
      ]

mkExample ∷ Effect (ReactComponent {})
mkExample = do
  closeicon <- CloseIcon.makeComponent
  centre <- Centre.makeComponent
  button <- mkButton
  component "CloseIconStory" \{} -> React.do
    state /\ modState <- useState { open: true }
    pure
      $ fragment
          [ jsx centre {}
              [ jsx button
                  { onClick: handler_ (modState (_ { open = true }))
                  }
                  [ R.text "Open CloseIcon" ]
              ]
          , R.p_ [ R.text "Here's some text to see what happens when there's actual content" ]
          , guard (state.open)
              $ element closeicon
                  { onClick: modState (_ { open = false })
                  , style: Nothing
                  }
          ]

mkAnimatedExample ∷ Effect (ReactComponent {})
mkAnimatedExample = do
  closeicon <- CloseIcon.makeComponent
  centre <- Centre.makeComponent
  button <- mkButton
  component "CloseIconStoryAnimated" \{} -> React.do
    state /\ modState <- useState { open: true }
    transitions <-
      useTransition [ state.open ] Nothing
        $ css
            { from: { opacity: 0.0, transform: "translate3d(-50%, -50%, 0) scale3d(0.0, 0.0, 1.0)" }
            , enter: { opacity: 1.0, transform: "translate3d(-50%, -50%, 0) scale3d(1.0, 1.0, 1.0)" }
            , leave: { opacity: 0.0, transform: "translate3d(-50%, -50%, 0) scale3d(0.1, 0.1, 1.0)" }
            }
    pure
      $ fragment
      $ [ jsx centre {}
            [ jsx button
                { onClick: handler_ $ modState _ { open = true }
                }
                [ R.text "Open CloseIcon" ]
            ]
        , R.p_ [ R.text "Here's some text to see what happens when there's actual content" ]
        ]
      <> ( transitions
            <#> \{ item, key, props } ->
                guard (item == Just true)
                  $ element closeicon
                      { onClick: modState _ { open = false }
                      , style: Just props
                      }
        )
