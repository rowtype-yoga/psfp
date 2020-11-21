module Yoga.Header.Stories where

import Prelude hiding (add)

import Effect (Effect)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.Header.Component (mkHeader)

-- What's about
stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "Header" do
    addDecorator fullScreenDecorator
    add "The Header" mkHeader
      [ { kids:
          [ R.text "Rowtype Yoga"
          ]
        , className: ""
        }
      ]
