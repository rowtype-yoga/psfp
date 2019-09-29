module Typography.Paragraph where

import Prelude
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Theme.Styles (makeStyles)
import Theme.Types (Theme)

mkP ∷ Effect (ReactComponent { text ∷ String })
mkP = do
  useStyles <-
    makeStyles \(theme ∷ Theme) ->
      { p:
        css
          { color: theme.textColour
          , fontFamily: theme.textFontFamily
          , fontSize: "1em"
          , margin: "0.67em 0 0.33em 0"
          , padding: "0.67em 0 0.33em 0"
          }
      }
  component "paragraph" \{ text } -> React.do
    classes <- useStyles
    pure
      $ R.p
          { className: classes.p
          , children: [ R.text text ]
          }
