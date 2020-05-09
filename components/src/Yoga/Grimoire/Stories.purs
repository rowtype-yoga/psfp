module Yoga.Grimoire.Stories where

import Prelude hiding (add)
import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Grimoire.Component as Grimoire

stories ∷ Effect Storybook
stories = do
  storiesOf "Grimoire" do
    addDecorator fullScreenDecorator
    add "The Grimoire" Grimoire.makeComponent
      [ justifill
          { spells }
      ]

spells =
  [ { name: "cast", signature: "String -> Effect Unit", description: "Casts an incantation" }
  , { name: "take", signature: "Int -> String -> String", description: "Takes the first characters of a string" }
  , { name: "append", signature: "a -> a -> a", description: "Takes two values and produces one" }
  , { name: "drop", signature: "Int -> String -> String", description: "Removes the first characters of a string wow man this is a really long description I bet it produces a much longer card than the others if I keep writing like a crazy person" }
  ]
