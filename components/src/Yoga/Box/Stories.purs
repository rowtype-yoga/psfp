module Yoga.Box.Stories where

import Prelude hiding (add)
import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Box.Component as Box

stories ∷ Effect Storybook
stories = do
  storiesOf "Box" do
    addDecorator fullScreenDecorator
    add "The Box" Box.makeComponent
      [ justifill
          { kids:
            [ R.text "Rowtype Yoga"
            , R.p_ [ R.text "This is a little bit longer my friend, I am telling you there was a lot to say in this case so it This is a little bit longer my friend, I am telling you there was a lot to say in this case so it wraps around" ]
            , R.h2_ [ R.text "I said Rowtype Yoga" ]
            , R.h3_ [ R.text "I said Rowtype Yoga" ]
            , R.p_ [ R.text "This is a little bit longer my friend, I am telling you there was a lot to say in this case so it This is a little bit longer my friend, I am telling you there was a lot to say in this case so it wraps around" ]
            ]
          }
      , justifill
          { kids:
            [ R.text "An Inverted box"
            , R.p_ [ R.text "This is a little bit longer my friend, I am telling you there was a lot to say in this case so it This is a little bit longer my friend, I am telling you there was a lot to say in this case so it wraps around" ]
            ]
          , invert: true
          }
      ]
