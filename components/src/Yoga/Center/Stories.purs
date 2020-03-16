module Yoga.Center.Stories where

import Prelude hiding (add)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Effect (Effect)
import Yoga.Center.Component as Center
import React.Basic.DOM as R
import Storybook.React (Storybook, add, addDecorator, storiesOf)

stories ∷ Effect Storybook
stories = do
  storiesOf "Center" do
    addDecorator fullScreenDecorator
    add "The Center" Center.makeComponent
      [ Center.withDefaults
          { kids:
            [ R.text "Rowtype Yoga"
            ]
          }
      ]
