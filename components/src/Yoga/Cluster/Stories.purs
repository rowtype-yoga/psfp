module Yoga.Cluster.Stories where

import Prelude hiding (add)
import Effect (Effect)
import Justifill (justifill)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Cluster.Component as Cluster

stories ∷ _ -> Effect Storybook
stories = do
  storiesOf "Cluster" do
    addDecorator fullScreenDecorator
    add "The Cluster" Cluster.makeComponent
      [ justifill
          { kids:
            [ R.ul
                { style:
                  css
                    { padding: "0"
                    , listStyle: "none"
                    }
                , children:
                  [ item "Number one"
                  , item "Number two"
                  , item "Number three"
                  , item "Number four"
                  , item "Number five"
                  , item "Number six"
                  , item "Number seven"
                  , item "Number eight"
                  , item "Number nine"
                  , item "Number ten"
                  , item "Number eleven"
                  , item "Number twelve"
                  ]
                }
            ]
          }
      ]
  where
  item ∷ String -> JSX
  item x = R.li_ [ R.text x ]
