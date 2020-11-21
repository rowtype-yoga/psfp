module Yoga.Template.Stories where

import Prelude hiding (add)

import Effect (Effect)
import Justifill (justifill)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.Template.Component as Template

stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "Template" do
    addDecorator fullScreenDecorator
    add "The Template" Template.makeComponent
      [ justifill
          {}
      ]
