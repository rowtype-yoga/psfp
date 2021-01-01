module Yoga.FillInTheGaps.Component where

import Prelude
import Color as Color
import Data.Array as A
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Literals.Undefined (undefined)
import Prim.Row (class Lacks)
import React.Basic (JSX, ReactComponent, element, elementKeyed)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (reactComponent)
import React.Basic.Hooks as React
import React.Basic.SyntaxHighlighter.Component (mkHighlighterTheme, syntaxHighlighter)
import Unsafe.Coerce (unsafeCoerce)
import Yoga (el)
import Yoga.Block as Block
import Yoga.Block.Atom.Button.Types as ButtonType
import Yoga.Block.Atom.CodeInput as CodeInput
import Yoga.DOM.Hook (useFocus)
import Yoga.FillInTheGaps.Logic (Segment(..), findFirstHoleIndex, holeToFiller, updateSegments)
import Yoga.Helpers ((?||))

visibleRange ∷ Array (Array Segment) -> { end ∷ Int, start ∷ Int }
visibleRange arr = { start, end }
  where
    start = A.findIndex (_ == [ Start ]) arr ?|| 0

    end = A.findIndex (_ == [ End ]) arr ?|| A.length arr

mkSegment ∷
  ∀ p.
  Lacks "children" p =>
  Lacks "key" p =>
  Lacks "ref" p =>
  Effect
    ( ReactComponent
        { focusOnFirstRender ∷ Boolean
        , maxLength ∷ Int
        , update ∷ String -> Effect Unit
        , text ∷ String
        , theKey ∷ String
        | p
        }
    )
mkSegment =
  reactComponent "Segment" \{ update, maxLength, text, theKey, focusOnFirstRender } -> React.do
    focusRef <- useFocus
    pure
      $ elementKeyed CodeInput.component
          { onChange: handler targetValue (traverse_ update)
          , maxLength
          , value: text
          , ref: if focusOnFirstRender then focusRef else (unsafeCoerce undefined)
          , key: theKey
          }

renderSegments ∷
  ReactComponent
    { focusOnFirstRender ∷ Boolean
    , maxLength ∷ Int
    , text ∷ String
    , theKey ∷ String
    , update ∷ String -> Effect Unit
    } ->
  ((Array (Array Segment) -> Array (Array Segment)) -> Effect Unit) -> Array (Array Segment) -> JSX
renderSegments segment update arrs = R.div_ (A.mapWithIndex renderLine arrs)
  where
    firstHoleIndex = findFirstHoleIndex arrs

    { start, end } = visibleRange arrs

    renderLine i l = R.div_ (A.mapWithIndex (renderSegment i) l)

    renderSegment i j s = case s, between start end i of
      Filler s', true ->
        element
          syntaxHighlighter
          { style:
            mkHighlighterTheme
              { grey: Color.rgb 50 50 50
              , highlightColour: Color.hsl 30.0 0.5 0.4
              , textColour: Color.rgb 0 0 1
              }
          , language: "purescript"
          , children: s'
          }
      Hole maxLength text, true ->
        element segment
          { maxLength
          , theKey: show i <> "," <> show j
          , update: update <<< updateSegments i j
          , text
          , focusOnFirstRender: (firstHoleIndex <#> \fh -> fh.i == i && fh.j == j) # fromMaybe false
          }
      _, _ -> mempty

type Props =
  { segments ∷ Array (Array Segment)
  , updateSegments ∷ (Array (Array Segment) -> Array (Array Segment)) -> Effect Unit
  , run ∷ Effect Unit
  , solvedWith ∷ Maybe String
  }

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  segment <- mkSegment
  reactComponent "FillInTheGaps" \(props@{ updateSegments, segments, run, solvedWith } ∷ Props) -> React.do
    ref <- useFocus
    pure
      $ R.form
          { onSubmit: handler preventDefault (const run)
          , children:
            [ el R.div' {}
                [ el Block.box {}
                    [ el Block.stack {}
                        [ case solvedWith of
                            Nothing -> renderSegments segment updateSegments segments
                            Just _ -> renderSegments segment updateSegments (segments <#> map holeToFiller)
                        , case solvedWith of
                            Just solution -> el R.pre' {} [ R.text solution ]
                            Nothing ->
                              el Block.cluster {}
                                [ R.div_
                                    [ el Block.button
                                        { onClick: handler_ mempty
                                        , buttonType: ButtonType.Primary
                                        , ref: if isJust (findFirstHoleIndex segments) then unsafeCoerce undefined else ref
                                        }
                                        [ R.text "Run" ]
                                    ]
                                ]
                        ]
                    ]
                ]
            ]
          }
