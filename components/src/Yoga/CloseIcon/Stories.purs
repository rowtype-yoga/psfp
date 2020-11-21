module Yoga.CloseIcon.Stories where

import Prelude hiding (add)
import Effect (Effect)
import Justifill (justifill)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, Stories, add, addDecorator, storiesOf)
import Web.HTML (window)
import Web.HTML.Window (alert)
import Yoga.CloseIcon.Component as CloseIcon

stories ∷ Stories
stories =
  storiesOf "CloseIcon" do
    addDecorator fullScreenDecorator
    add "The CloseIcon" CloseIcon.makeComponent
      [ justifill
          { onClick: window >>= alert "Clicked close"
          }
      ]
