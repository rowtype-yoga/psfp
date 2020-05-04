module Yoga.Cover.Stories where

import Prelude hiding (add)
import Effect (Effect)
import Justifill (justifill)
import React.Basic (JSX)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Cover.Component as Cover

stories ∷ Effect Storybook
stories = do
  storiesOf "Cover" do
    addDecorator fullScreenDecorator
    add "The Cover" Cover.makeComponent
      [ justifill
          { kids: [ R.h1_ [ R.text "Main thing" ] ]
          , header: R.text "Header"
          , footer: R.text "Footer"
          }
      , justifill
          { kids: [ R.h1_ [ R.text "Only main" ] ]
          }
      , justifill
          { header: R.h1_ [ R.text "Only header" ]
          , kids: [] ∷ Array JSX
          }
      , justifill
          { header: R.h1_ [ R.text "Header" ]
          , footer: R.h1_ [ R.text "Footer" ]
          , kids: [] ∷ Array JSX
          }
      ]
