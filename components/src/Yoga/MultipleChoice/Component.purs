module Yoga.MultipleChoice.Component where

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
import Yoga.Card.Component (mkCard)
import Yoga.Cluster.Component as Cluster
import Yoga.DOM.Hook (useFocus)
import Yoga.Helpers ((?||))
import Yoga.InlineCode.Component as InlineCode
import Yoga.MultipleChoice.Logic (Segment(..), complete, findFirstHoleIndex, holeToFiller, updateSegments)
import Yoga.MultipleChoice.Styles (styles)
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (makeStylesJSS, useTheme)
import Yoga.Theme.Syntax (mkHighlighterTheme)
import Yoga.Theme.Types (CSSTheme)

type Props
  = { segments ∷ Array (Array Segment)
    , updateSegments ∷ (Array (Array Segment) -> Array (Array Segment)) -> Effect Unit
    , incantate ∷ Effect Unit
    , solvedWith ∷ Maybe String
    }

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  card <- mkCard
  useStyles <- makeStylesJSS styles
  component "MultipleChoice" \(props@{ updateSegments, segments, incantate, solvedWith } ∷ Props) -> React.do
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
      $ jsx card {} [ R.text "Hio!" ]
