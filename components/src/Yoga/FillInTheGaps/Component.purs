module Yoga.FillInTheGaps.Component where

import Prelude
import Data.Array as A
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Justifill (justifill)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as R
import React.Basic.Hooks (component, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import React.Basic.SyntaxHighlighter.Component (HighlighterTheme, syntaxHighlighterImpl)
import Yoga.FillInTheGaps.Logic (Segment(..), updateSegments)
import Yoga.Helpers ((?||))
import Yoga.InlineCode.Component as InlineCode
import Yoga.Theme.Styles (useTheme)
import Yoga.Theme.Syntax (mkHighlighterTheme)

visibleRange ∷ Array (Array Segment) -> { end ∷ Int, start ∷ Int }
visibleRange arr = { start, end }
  where
  start = A.findIndex (_ == [ Start ]) arr ?|| 0
  end = A.findIndex (_ == [ End ]) arr ?|| A.length arr

renderSegments ∷ HighlighterTheme -> ReactComponent InlineCode.Props -> ((Array (Array Segment) -> Array (Array Segment)) -> Effect Unit) -> Array (Array Segment) -> JSX
renderSegments highlighterTheme ic update arrs = R.div_ (A.mapWithIndex renderLine arrs)
  where
  { start, end } = visibleRange arrs
  renderLine i l = R.div_ (A.mapWithIndex (renderSegment i) l)
  renderSegment i j s =
    if between start end i then case s of
      Filler s' -> element syntaxHighlighterImpl { style: highlighterTheme, language: "purescript", children: s' }
      Hole width _ ->
        element ic
          $ justifill
              { width
              , onSubmit: update <<< updateSegments i j
              }
      _ -> mempty
    else
      mempty

makeComponent ∷ Effect (ReactComponent { initialSegments ∷ Array (Array Segment), update ∷ Array (Array Segment) -> Effect Unit })
makeComponent = do
  ic <- InlineCode.makeComponent
  component "FillInTheGaps" \{ initialSegments, update } -> React.do
    segments /\ modifySegments <- useState initialSegments
    useAff segments do
      delay (200.0 # Milliseconds)
      update segments # liftEffect
    highlighterTheme <- useTheme <#> mkHighlighterTheme
    pure $ renderSegments highlighterTheme ic (modifySegments) segments
