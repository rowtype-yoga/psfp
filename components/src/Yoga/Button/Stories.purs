module Yoga.Button.Stories where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Justifill (justifill)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (reactComponent)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.Box.Component as Box
import Yoga.Button.Component (ButtonType(..))
import Yoga.Button.Component as Button

stories ∷ NodeModule -> Effect Storybook
stories =
  storiesOf "Button" do
    addDecorator fullScreenDecorator
    add "Button" mkExample
      [ { text: "Cancel"
        , onClick: handler_ (log "clicked cancel")
        , buttonType: Nothing
        }
      , { text: "Okay"
        , buttonType: Just $ HighlightedButton
        , onClick: handler_ (log "clicked OK")
        }
      , { text: "Disabled"
        , buttonType: Just DisabledButton
        , onClick: handler_ (log "clicked Disabled")
        }
      , justifill
          { text: "Very long button text"
          , onClick: handler_ (log "clicked very long...")
          }
      ]
  where
  mkExample = do
    box <- Box.makeComponent
    button <- Button.mkButton
    reactComponent "ExampleButton" \{ text, buttonType, onClick } -> React.do
      pure
        $ jsx button
            { onClick
            , buttonType
            }
            [ R.text text ]

loremIpsum ∷ String
loremIpsum =
  """PureScript is a strongly-typed, purely-functional programming language that compiles"""
    <> """ to JavaScript. It can be used to develop web applications, server side apps, and al"""
    <> """so desktop applications with use of Electron. Its syntax is mostly comparable to tha"""
    <> """t of Haskell. In addition, it introduces row polymorphism and extensible records.[2]"""
    <> """ Also, contrary to Haskell, PureScript adheres to a strict evaluation strategy."""
