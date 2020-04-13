module Yoga.FillInTheGaps.Stories where

import Prelude hiding (add)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.FillInTheGaps.Component as FillInTheGaps

ctx ∷ { | FillInTheGaps.Ctx () }
ctx =
  { compileAndRun
  }
  where
  compileAndRun = case _ of
    { code }
      | code == correctCode -> pure (Right { code: Nothing, stdout: "Hello World\n", stderr: "" })
    other -> pure (Right { code: Nothing, stdout: "Not hello world", stderr: "" })

stories ∷ Effect Storybook
stories = do
  storiesOf "FillInTheGaps" do
    addDecorator fullScreenDecorator
    add "The FillInTheGaps" (FillInTheGaps.makeComponent ctx)
      [ { code: codeWithHoles }
      ]

codeWithHoles =
  """
--result Hello World
module Main where
import Grimoire

main :: Effect Unit
--start here
main = cast
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
