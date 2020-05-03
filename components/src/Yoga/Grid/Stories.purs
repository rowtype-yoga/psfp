module Yoga.Grid.Stories where

import Prelude hiding (add)
import Data.Array (mapWithIndex, replicate, (..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Justifill (justifill)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (component, element)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Box.Component as Box
import Yoga.Card.Component (mkCard)
import Yoga.Centre.Component as Centre
import Yoga.Grid.Component as Grid
import Yoga.Stack.Component as Stack

stories ∷ Effect Storybook
stories = do
  storiesOf "Grid" do
    addDecorator fullScreenDecorator
    add "The Grid" mkExample
      [ justifill
          { kids:
            (1 .. 11)
              <#> \i ->
                  R.div
                    { style:
                      css
                        { background: "url(" <> src i <> ")"
                        , width: "100%"
                        , height: "200px"
                        , backgroundSize: "cover"
                        }
                    }
          , minWidth: "200px"
          }
      ]
  where
  mkExample = do
    box <- Box.makeComponent
    grid <- Grid.makeComponent
    centre <- Centre.makeComponent
    card <- mkCard
    component "ExampleGrid" \(props ∷ Grid.Props) -> React.do
      let
        kids = props.kids <#> \kid -> jsx card {} [ kid ]
      pure
        $ R.div
            { style:
              css { width: "100vw" }
            , children:
              [ jsx box {} [ element grid (props { kids = kids }) ]
              ]
            }

src seed = "https://picsum.photos/200?random=" <> show seed
