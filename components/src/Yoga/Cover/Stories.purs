module Yoga.Cover.Stories where

import Prelude hiding (add)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Effect (Effect)
import Yoga.Cover.Component as Cover
import React.Basic.DOM as R
import Storybook.React (Storybook, add, addDecorator, storiesOf)

stories ∷ Effect Storybook
stories = do
  storiesOf "Cover" do
    addDecorator fullScreenDecorator
    add "The Cover" Cover.makeComponent
      [ Cover.withDefaults
          { principal: R.h1_ [ R.text "Main thing" ]
          , header: R.text "Header"
          , footer: R.text "Footer"
          }
      , Cover.withDefaults
          { principal: R.h1_ [ R.text "Only main" ]
          }
      , Cover.withDefaults
          { header: R.h1_ [ R.text "Only header" ]
          }
      , Cover.withDefaults
          { header: R.h1_ [ R.text "Header" ]
          , footer: R.h1_ [ R.text "Footer" ]
          }
      ]
