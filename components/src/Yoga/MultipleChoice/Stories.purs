module Yoga.MultipleChoice.Stories where

import Prelude hiding (add)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Ref as Ref
import Justifill (justifill)
import React.Basic (ReactComponent, fragment)
import React.Basic.Hooks (element, useState, component)
import React.Basic.Hooks as React
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.MultipleChoice.Component as MultipleChoice
import Yoga.MultipleChoice.Logic (parseSegments)
import Yoga.Grimoire.Component as Grimoire
import Yoga.Grimoire.Spell.Component as Spell
import Yoga.Helpers ((?||))
import Yoga.Spell.Types (Spell)
import Yoga.WithSidebar.Component as WithSidebar

stories ∷ _ -> Effect Storybook
stories = do
  storiesOf "MultipleChoice" do
    addDecorator fullScreenDecorator
    segRef <- Ref.new [] # lift
    add "The MultipleChoice" makeWrapper
      [ {} ]

makeWrapper ∷ Effect (ReactComponent {})
makeWrapper = do
  gaps <- MultipleChoice.makeComponent
  withSidebar <- WithSidebar.makeComponent
  spell <- Spell.makeComponent
  component "GapsWrapper" \_ -> React.do
    segments /\ updateSegments <- useState $ parseSegments codeWithHoles ?|| []
    pure
      $ element withSidebar
          ( justifill
              { notSidebarChildren:
                [ element gaps
                    { segments
                    , updateSegments
                    , incantate: mempty
                    , solvedWith: Nothing
                    }
                ]
              , sidebarChildren: [ element spell { spell: castSpell } ]
              }
          )

castSpell ∷ Spell
castSpell = { name: "cast", signature: "String -> Effect Unit", description: "Casts an incantation" }

spells ∷ Array Spell
spells =
  [ { name: "cast", signature: "String -> Effect Unit", description: "Casts an incantation" }
  , { name: "take", signature: "Int -> String -> String", description: "Takes the first characters of a string" }
  , { name: "append", signature: "a -> a -> a", description: "Takes two values and produces one" }
  , { name: "drop", signature: "Int -> String -> String", description: "Removes the first characters of a string wow man this is a really long description I bet it produces a much longer card than the others if I keep writing like a crazy person" }
  ]

codeWithHoles =
  """
--result Hello World
module Main where
import Grimoire

incantation :: Effect Unit
--start here
--please say Hello World
incantation = cast
  "{-Hello World-}"
--end here
"""
