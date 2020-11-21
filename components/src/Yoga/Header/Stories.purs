module Yoga.Header.Stories where

import Prelude hiding (add)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Effect (Effect)
import Yoga.Header.Component (mkHeader)
import React.Basic.DOM as R
import Storybook.React (Storybook, add, addDecorator, storiesOf)

-- What's about
stories ∷ _ -> Effect Storybook
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
