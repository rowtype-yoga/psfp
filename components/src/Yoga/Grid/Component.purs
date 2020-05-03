module Yoga.Grid.Component where

import Prelude
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM as R
import React.Basic.Hooks (component)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Grid.Styles (styles)
import Yoga.Grid.Styles as Style
import Yoga.Theme.Styles (makeStylesJSS, useTheme)
import Yoga.Theme.Types (CSSTheme)

type Props
  = { kids ∷ Array JSX
    , className ∷ Maybe String
    | Style.PropsR
    }

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS styles
  component "Grid" \(props@{ kids, className } ∷ Props) -> React.do
    { grid } <- useStyles (pick props)
    theme ∷ CSSTheme <- useTheme
    pure
      $ R.div
          { className: grid
          , children: kids
          }
