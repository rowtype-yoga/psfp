module Yoga.FillInTheGaps.Component where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect (Effect)
import Justifill (justifill)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (component)
import React.Basic.Hooks as React
import React.Basic.SyntaxHighlighter.Component (HighlighterTheme, syntaxHighlighterImpl)
import Yoga.Button.Component (ButtonType(..), mkButton)
import Yoga.Cluster.Component as Cluster
import Yoga.FillInTheGaps.Logic (Segment(..), complete, updateSegments)
import Yoga.Helpers (ifJustTrue, (?||))
import Yoga.InlineCode.Component as InlineCode
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (useTheme)
import Yoga.Theme.Syntax (mkHighlighterTheme)

visibleRange ∷ Array (Array Segment) -> { end ∷ Int, start ∷ Int }
visibleRange arr = { start, end }
  where
  start = A.findIndex (_ == [ Start ]) arr ?|| 0
  end = A.findIndex (_ == [ End ]) arr ?|| A.length arr

renderSegments ∷ Maybe Boolean -> HighlighterTheme -> ReactComponent InlineCode.Props -> ((Array (Array Segment) -> Array (Array Segment)) -> Effect Unit) -> Array (Array Segment) -> JSX
renderSegments readOnly highlighterTheme ic update arrs = R.div_ (A.mapWithIndex renderLine arrs)
  where
  { start, end } = visibleRange arrs
  renderLine i l = R.div_ (A.mapWithIndex (renderSegment i) l)
  renderSegment i j s =
    if between start end i then case s of
      Filler s' -> element syntaxHighlighterImpl { style: highlighterTheme, language: "purescript", children: s' }
      Hole width text ->
        element ic
          $ justifill
              { width
              , update: update <<< updateSegments i j
              , text
              , readOnly
              }
      _ -> mempty
    else
      mempty

type Props
  = { segments ∷ Array (Array Segment)
    , updateSegments ∷ (Array (Array Segment) -> Array (Array Segment)) -> Effect Unit
    , incantate ∷ Effect Unit
    , readOnly ∷ Maybe Boolean
    }

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  ic <- InlineCode.makeComponent
  btn <- mkButton
  stack <- Stack.makeComponent
  cluster <- Cluster.makeComponent
  component "FillInTheGaps" \({ updateSegments, segments, incantate, readOnly } ∷ Props) -> React.do
    highlighterTheme <- useTheme <#> mkHighlighterTheme
    pure
      $ jsx stack {}
          [ renderSegments readOnly highlighterTheme ic updateSegments segments
          , if Just true == readOnly then
              mempty
            else
              jsx cluster {}
                [ R.div_
                    [ jsx btn
                        { onClick: handler_ incantate
                        , buttonType: if complete segments then HighlightedButton else DisabledButton
                        }
                        [ R.text "Incantate" ]
                    ]
                ]
          ]
