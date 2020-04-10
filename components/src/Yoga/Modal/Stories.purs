module Yoga.Modal.Stories where

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
import React.Basic.Hooks (ReactComponent, component, element, fragment, memo, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (useTransition)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Button.Component (mkButton)
import Yoga.Centre.Component as Centre
import Yoga.CloseIcon.Component as CloseIcon
import Yoga.Modal.Component as Modal

stories ∷ Effect Storybook
stories = do
  storiesOf "Modal" do
    addDecorator fullScreenDecorator
    add "The Modal" Modal.makeComponent
      [ justifill
          { kids: [ R.text "I'm a Modal" ]
          , title: "Something important has happened this is excessively, if not prohibitively long title text to see what will happen"
          }
      ]
    add "Interactive Modal Example" mkExample
      [ {}
      ]
    add "Interactive Animated Modal Example" mkAnimatedExample
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
              $ jsx modal
                  { title: "Warning"
                  }
                  [ R.text "This is more interactive"
                  ]
          ]

mkAnimatedExample ∷ Effect (ReactComponent {})
mkAnimatedExample = do
  modal <- Modal.makeComponent
  centre <- Centre.makeComponent
  closeIcon <- memo CloseIcon.makeComponent
  button <- mkButton
  component "ModalStoryAnimated" \{} -> React.do
    state /\ modState <- useState { open: true }
    transitions <-
      useTransition [ state.open ] (Just $ show)
        $ css
            { from: { opacity: 0.0, transform: "translate3d(-50%, -50%, 0) scale3d(0.3, 0.3, 1.0)" }
            , enter: { opacity: 1.0, transform: "translate3d(-50%, -50%, 0) scale3d(1.0, 1.0, 1.0)" }
            , leave: { opacity: 0.0, transform: "translate3d(-50%, -50%, 0) scale3d(0.3, 0.3, 1.0)" }
            , config: { mass: 1.0, tension: 170, friction: 20 }
            }
    pure
      $ fragment
      $ [ jsx centre {}
            [ jsx button
                { onClick: handler_ $ modState _ { open = true }
                }
                [ R.text "Open Modal" ]
            ]
        , R.p_ [ R.text "Here's some text to see what happens when there's actual content" ]
        ]
      <> ( transitions
            <#> \{ item, key, props } ->
                guard (item == Just true)
                  $ jsx modal
                      { title: "Warning"
                      , icon:
                        element closeIcon
                          $ justifill
                              { onClick: modState _ { open = false }
                              }
                      , style: Just props
                      }
                      [ R.text "This is more interactive"
                      ]
        )
