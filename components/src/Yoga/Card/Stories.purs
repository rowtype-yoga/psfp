module Yoga.Card.Stories where

import Prelude hiding (add)

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (reactComponent)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.Box.Component as Box
import Yoga.Card.Component (mkCard)

stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "Card" do
    addDecorator fullScreenDecorator
    add "Example card" mkExample
      [ {}
      ]
  where
  mkExample = do
    box <- Box.makeComponent
    card <- mkCard
    reactComponent "ExampleCard" \{} -> React.do
      pure
        $ jsx box {}
        $ pure
        $ jsx card {}
            [ jsx box {} [ R.text "hi there!" ]
            ]

loremIpsum ∷ String
loremIpsum =
  """PureScript is a strongly-typed, purely-functional programming language that compiles"""
    <> """ to JavaScript. It can be used to develop web applications, server side apps, and al"""
    <> """so desktop applications with use of Electron. Its syntax is mostly comparable to tha"""
    <> """t of Haskell. In addition, it introduces row polymorphism and extensible records.[2]"""
    <> """ Also, contrary to Haskell, PureScript adheres to a strict evaluation strategy."""
