module Yoga.FillInTheGaps.Stories where

import Prelude hiding (add)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import React.Basic (JSX, ReactComponent)
import React.Basic.Hooks (element, reactComponent, useState)
import React.Basic.Hooks as React
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Yoga.FillInTheGaps.Component as FillInTheGaps
import Yoga.FillInTheGaps.Logic (Segment, parseSegments)

default ∷
  { decorators ∷ Array (Effect JSX -> JSX)
  , title ∷ String
  }
default =
  { title: "FillInTheGaps"
  , decorators:
    [ \storyFn ->
        unsafePerformEffect (fullScreenDecorator storyFn)
    ]
  }

stories ∷ Effect JSX
stories = do
  wrapper <- makeWrapper
  pure $ React.element wrapper {}
  where
    makeWrapper ∷ Effect (ReactComponent {})
    makeWrapper = do
      gaps <- FillInTheGaps.makeComponent
      reactComponent "GapsWrapper" \_ -> React.do
        segments /\ updateSegments <- useState initialSegments
        pure
          $ element gaps
              { segments: segments
              , updateSegments
              , run: log "pressed run"
              , solvedWith: Nothing
              }

    initialSegments ∷ Array (Array Segment)
    initialSegments = parseSegments codeWithHoles # unsafePartial fromJust

    codeWithHoles ∷ String
    codeWithHoles =
      """
--result kazam
module Main where
import Grimoire

main :: Effect Unit
main = 
  --start here
  {-log-} "kazam"
  log "{-ala-}"
--end here
"""
