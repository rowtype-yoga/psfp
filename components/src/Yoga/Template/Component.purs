module Yoga.Template.Component where

import Prelude
import Effect (Effect)
import React.Basic (ReactComponent)
import React.Basic.DOM as R
import React.Basic.Hooks (component)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Template.Styles (styles)
import Yoga.Template.Styles as Style
import Yoga.Theme.Styles (makeStylesJSS, useTheme)
import Yoga.Theme.Types (CSSTheme)

type Props
  = { 
    | Style.PropsR
    }

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS styles
  component "Template" \(props@{} ∷ Props) -> React.do
    {} <- useStyles (pick props)
    theme ∷ CSSTheme <- useTheme
    pure
      $ R.div_ [ R.text "WriteMe" ]
