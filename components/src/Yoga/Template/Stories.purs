module Yoga.Template.Stories where

import Prelude hiding (add)
import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Template.Component as Template

stories ∷ Effect Storybook
stories = do
  storiesOf "Template" do
    addDecorator fullScreenDecorator
    add "The Template" Template.makeComponent
      [ justifill
          {}
      ]
