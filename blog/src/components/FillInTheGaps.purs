module FillInTheGaps where

import Prelude

import Data.Array as A
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Justifill (justifill)
import Partial.Unsafe (unsafeCrashWith)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as R
import React.Basic.Hooks (component, useState)
import React.Basic.Hooks as React
import Yoga.Helpers (intersperse)
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
    
mkFillInTheGaps :: Effect (ReactComponent { code :: String })
mkFillInTheGaps = do 
  ic <- InlineCode.makeComponent
  component "FillInTheGaps" \{ code } -> React.do 
    let 
      separator = "???"
      lines = split (Pattern "\n") code
      rawSegments = split (Pattern separator) >>> intersperse separator
      toSegment = case _ of
        x | x == separator -> Hole ""
        other -> Filler other
      initialSegments = lines <#> \line -> rawSegments line <#> toSegment
    segments /\ modifySegments <- useState initialSegments
    pure $ renderSegments ic (modifySegments) segments