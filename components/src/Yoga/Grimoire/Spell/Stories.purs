module Yoga.Grimoire.Spell.Stories where

import Prelude hiding (add)

import Effect (Effect)
import Justifill (justifill)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.Grimoire.Spell.Component as Spell

stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "Spell" do
    addDecorator fullScreenDecorator
    add "The Spell" Spell.makeComponent
      [ justifill
          { spell:
            { name: "append", signature: "a -> a -> a", description: "Takes two values and produces one" }
          }
      ]
