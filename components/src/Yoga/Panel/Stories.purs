module Panel.Stories where

import Prelude hiding (add)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Effect (Effect)
import Yoga.Panel.Component as Panel
import React.Basic.DOM as R
import Record (disjointUnion)
import Storybook.React (Storybook, add, addDecorator, storiesOf)

stories ∷ Effect Storybook
stories = do
  storiesOf "Panel" do
    addDecorator fullScreenDecorator
    add "The Panel" Panel.makeComponent
      [ { kids:
          [ R.text "Rowtype Yoga"
          ]
        }
          `disjointUnion`
            mempty
      ]
