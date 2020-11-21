module Yoga.FillInTheGapsDraggable.Component where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype as NT
import Data.Nullable (Nullable)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Justifill (justifill)
import Literals.Undefined (undefined)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (Ref, component)
import React.Basic.Hooks as React
import React.Basic.SyntaxHighlighter.Component (HighlighterTheme, syntaxHighlighterImpl)
import Record.Extra (pick)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)
import Yoga.Box.Component as Box
import Yoga.Button.Component (ButtonType(..), mkButtonWithProps)
import Yoga.Button.Component as Button
import Yoga.Cluster.Component as Cluster
import Yoga.DOM.Hook (useFocus)
import Yoga.FillInTheGapsDraggable.Logic (Segment(..), complete, findFirstHoleIndex, holeToFiller, updateSegments)
import Yoga.FillInTheGapsDraggable.Styles (styles)
import Yoga.Helpers ((?||))
import Yoga.InlineCode.Component as InlineCode
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (makeStylesJSS, useTheme)
import Yoga.Theme.Syntax (mkHighlighterTheme)
import Yoga.Theme.Types (CSSTheme)

visibleRange ∷ Array (Array Segment) -> { end ∷ Int, start ∷ Int }
visibleRange arr = { start, end }
  where
  start = A.findIndex (_ == [ Start ]) arr ?|| 0
  end = A.findIndex (_ == [ End ]) arr ?|| A.length arr

renderSegments ∷ Milliseconds -> HighlighterTheme -> ReactComponent InlineCode.Props -> ((Array (Array Segment) -> Array (Array Segment)) -> Effect Unit) -> Array (Array Segment) -> JSX
renderSegments debounceBy highlighterTheme ic update arrs = R.div_ (A.mapWithIndex renderLine arrs)
  where
  firstHoleIndex = findFirstHoleIndex arrs
  { start, end } = visibleRange arrs
  renderLine i l = R.div_ (A.mapWithIndex (renderSegment i) l)
  renderSegment i j s = case s, between start end i of
    Filler s', true ->
      element
        syntaxHighlighterImpl
        { style: highlighterTheme, language: "purescript", children: s' }
    Hole width text, true ->
      element ic
        $ justifill
            { width
            , update: update <<< updateSegments i j
            , text
            , focusOnFirstRender: firstHoleIndex <#> \fh -> fh.i == i && fh.j == j
            }
    _, _ -> mempty

type Props
  = { segments ∷ Array (Array Segment)
    , updateSegments ∷ (Array (Array Segment) -> Array (Array Segment)) -> Effect Unit
    , incantate ∷ Effect Unit
    , solvedWith ∷ Maybe String
    }

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  ic <- InlineCode.makeComponent
  btn <- mkButtonWithProps ∷ Button.WithProps ( ref ∷ Ref (Nullable Node) )
  stack <- Stack.makeComponent
  box <- Box.makeComponent
  cluster <- Cluster.makeComponent
  useStyles <- makeStylesJSS styles
  component "FillInTheGapsDraggable" \(props@{ updateSegments, segments, incantate, solvedWith } ∷ Props) -> React.do
    { codeContainer, solutionContainer } <- useStyles (pick props)
    theme ∷ CSSTheme <- useTheme
    ref <- useFocus
    let
      highlighterTheme = mkHighlighterTheme theme
      debounceBy = 16.6666667 # Milliseconds
      onSubmitHandler _ =
        launchAff_ do
          delay (NT.over2 Milliseconds (+) debounceBy (5.0 # Milliseconds))
          incantate # liftEffect
    pure
      $ R.form
          { onSubmit: handler preventDefault onSubmitHandler
          , children:
            [ R.div
                { className: codeContainer
                , children:
                  [ jsx box { className: codeContainer }
                      [ jsx stack {}
                          [ case solvedWith of
                              Nothing -> renderSegments debounceBy highlighterTheme ic updateSegments segments
                              Just _ -> renderSegments debounceBy highlighterTheme ic updateSegments (segments <#> map holeToFiller)
                          , case solvedWith of
                              Just solution ->
                                R.pre
                                  { className: solutionContainer
                                  , children: [ R.text solution ]
                                  }
                              Nothing ->
                                jsx cluster {}
                                  [ R.div_
                                      [ jsx btn
                                          { onClick: handler_ mempty
                                          , buttonType: if complete segments then HighlightedButton else DisabledButton
                                          , buttonProps: { ref: if isJust (findFirstHoleIndex segments) then unsafeCoerce undefined else ref }
                                          }
                                          [ R.text "Incantate" ]
                                      ]
                                  ]
                          ]
                      ]
                  ]
                }
            ]
          }
