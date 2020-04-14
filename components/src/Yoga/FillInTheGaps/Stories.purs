module Yoga.FillInTheGaps.Stories where

import Prelude hiding (add)
import Data.String as String
import Effect (Effect)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.FillInTheGaps.Component as FillInTheGaps
import Yoga.FillInTheGaps.Logic (parseSegments)
import Yoga.Helpers ((?||))

stories ∷ Effect Storybook
stories = do
  storiesOf "FillInTheGaps" do
    addDecorator fullScreenDecorator
    add "The FillInTheGaps" (FillInTheGaps.makeComponent)
      [ { initialSegments: parseSegments codeWithHoles ?|| [], update: pure (pure unit) }
      ]

codeWithHoles =
  """
--result Hello World
module Main where
import Grimoire

incantation :: Effect Unit
--start here
incantation = cast
  "{-Hello World-}"
--end here
"""

correctCode =
  String.replace
    (String.Pattern "{-Hello World-}")
    (String.Replacement "Hello World")
    <<< String.replace
        (String.Pattern "--result Hello World")
        (String.Replacement "")
    <<< String.replace
        (String.Pattern "--start here")
        (String.Replacement "")
    <<< String.replace
        (String.Pattern "--end here")
        (String.Replacement "")
    $ codeWithHoles
