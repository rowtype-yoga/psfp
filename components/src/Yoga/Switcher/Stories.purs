module Yoga.Switcher.Stories where

import Prelude hiding (add)

import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.Switcher.Component as Switcher

stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "Switcher" do
    addDecorator fullScreenDecorator
    add "The Switcher" Switcher.makeComponent
      [ justifill
          { kids:
            [ R.text "Rowtype Yoga"
            , R.text "This is more"
            , R.text "Final one"
            , R.text "This is more"
            , R.text "Final one"
            , R.text "This is more"
            , R.text "Final one"
            , R.button_ [ R.text "Where is this?" ]
            ]
          , limit: 12
          }
      ]
