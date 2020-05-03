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
          {}
      ]
