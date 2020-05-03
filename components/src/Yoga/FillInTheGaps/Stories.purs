module Yoga.FillInTheGaps.Stories where

import Prelude hiding (add)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Ref as Ref
import React.Basic (ReactComponent)
import React.Basic.Hooks (element, useState, component)
import React.Basic.Hooks as React
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.FillInTheGaps.Component as FillInTheGaps
import Yoga.FillInTheGaps.Logic (parseSegments)
import Yoga.Helpers ((?||))

stories ∷ Effect Storybook
stories = do
  storiesOf "FillInTheGaps" do
    addDecorator fullScreenDecorator
    segRef <- Ref.new [] # lift
    add "The FillInTheGaps" makeWrapper
      [ {} ]

makeWrapper ∷ Effect (ReactComponent {})
makeWrapper = do
  gaps <- FillInTheGaps.makeComponent
  component "GapsWrapper" \_ -> React.do
    segments /\ updateSegments <- useState (parseSegments codeWithHoles ?|| [])
    pure
      $ element gaps
          { segments
          , updateSegments
          , incantate: mempty
          , solvedWith: Nothing
          }

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
