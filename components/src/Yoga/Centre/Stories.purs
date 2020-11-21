module Yoga.Centre.Stories where

import Prelude hiding (add)

import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.Centre.Component as Centre

stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "Centre" do
    addDecorator fullScreenDecorator
    add "The Centre" Centre.makeComponent
      [ justifill
          { kids:
            [ R.text "Rowtype Yoga"
            ]
          , andText: true
          }
      ]
