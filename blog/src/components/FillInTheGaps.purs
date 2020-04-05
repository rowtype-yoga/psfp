module FillInTheGaps where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Justifill (justifill)
import Partial.Unsafe (unsafeCrashWith)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactChildren, componentWithChildren, reactChildrenToArray, useState)
import React.Basic.Hooks as React
import Yoga.InlineCode.Component as InlineCode

data Segment = Filler String | Hole String

renderSegments :: ReactComponent InlineCode.Props -> ((Array (Array Segment) -> Array (Array Segment)) -> Effect Unit) -> Array (Array Segment) -> JSX
renderSegments ic update arrs = R.div_ (arrs `flip A.mapWithIndex` renderLine)
  where
  renderLine i l = R.div_ (l `flip A.mapWithIndex` renderSegment i)
  renderSegment i j = case _ of
    Filler s -> R.span_ [R.text s]
    Hole _ -> element ic $ justifill { onSubmit: \s -> update (updateSegments i j s) }

updateSegments :: Int -> Int -> String -> Array (Array Segment) -> Array (Array Segment)
updateSegments i j v arrs = fromMaybe [] (A.modifyAt i (updateLine j v) arrs)

updateLine :: Int -> String -> Array Segment -> Array Segment
updateLine idx v arr = fromMaybe [] (A.modifyAt idx f arr)
  where
  f = case _ of
    Filler s -> unsafeCrashWith "Updated a filler"
    Hole _ -> Hole v
    

fillInTheGaps :: ReactComponent { children :: ReactChildren String }
fillInTheGaps = unsafePerformEffect do 
  ic <- InlineCode.makeComponent
  componentWithChildren "InlineCodeWithChildren" \{ children } -> React.do 
    let 
      separator = "???"
      child = children # reactChildrenToArray # A.head # fromMaybe "gurki"
      lines = split (Pattern "\n") child
      rawSegments = split (Pattern separator)
      toSegment = case _ of
        x | x == separator -> Hole ""
        other -> Filler other
      initialSegments = lines <#> \line -> rawSegments line <#> toSegment
    (segments :: Array (Array Segment)) /\ modifySegments <- useState initialSegments
    pure $ renderSegments ic (modifySegments) segments