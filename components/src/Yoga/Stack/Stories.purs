module Yoga.Stack.Stories where

import Prelude hiding (add)

import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.Stack.Component as Stack

stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "Stack" do
    addDecorator fullScreenDecorator
    add "The Stack" Stack.makeComponent
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
            [ R.text "Rowtype Yoga"
            , R.p_ [ R.text "This is a little bit longer my friend, I am telling you there was a lot to say in this case so it This is a little bit longer my friend, I am telling you there was a lot to say in this case so it wraps around" ]
            , R.p_ [ R.text "This is a little bit longer my friend, I am telling you there was a lot to say in this case so it This is a little bit longer my friend, I am telling you there was a lot to say in this case so it wraps around" ]
            ]
          , space: "8rem"
          }
      , justifill
          { kids:
            [ R.text "Rowtype Yoga"
            , R.p_ [ R.text "This is a little bit longer my friend, I am telling you there was a lot to say in this case so it This is a little bit longer my friend, I am telling you there was a lot to say in this case so it wraps around" ]
            , R.p_ [ R.text "This is a little bit longer my friend, I am telling you there was a lot to say in this case so it This is a little bit longer my friend, I am telling you there was a lot to say in this case so it wraps around" ]
            ]
          , space: "8rem"
          , splitAfter: 1
          }
      ]
