module Yoga.MultipleChoice.Component where

import Prelude
import Data.Maybe (Maybe)
import Data.Newtype as NT
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import React.Basic (ReactComponent)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (reactComponent)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Card.Component (mkCard)
import Yoga.DOM.Hook (useFocus)
import Yoga.MultipleChoice.Logic (Segment)
import Yoga.MultipleChoice.Styles (styles)
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
  reactComponent "MultipleChoice" \(props@{ updateSegments, segments, incantate, solvedWith } ∷ Props) -> React.do
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
