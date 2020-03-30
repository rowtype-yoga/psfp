module Yoga.Typography.Paragraph where

import Prelude
import Color (toHexString)
import Effect (Effect)
import JSS (jssClasses)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

mkP ∷ Effect (ReactComponent { text ∷ String })
mkP = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { p:
            { color: theme.textColour # toHexString
            , fontSize: "1em"
            , margin: "0.67em 0 0.33em 0"
            , padding: "0.67em 0 0.33em 0"
            }
          }
  component "paragraph" \{ text } -> React.do
    classes <- useStyles {}
    pure
      $ R.p
          { className: classes.p
          , children: [ R.text text ]
          }
