module Container.Header where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import SVG.Icon (pslogoIcon)
import Theme.Styles (makeStyles)
import Theme.Types (Theme)
import Typography.Header (HeadingLevel(..), mkH)

mkHeader ∷ Effect (ReactComponent {})
mkHeader = do
  h <- mkH
  useStyles <-
    makeStyles \(theme ∷ Theme) ->
      { sidebar:
        css
          { backgroundColor: theme.backgroundColour
          , borderBottom: "1px solid " <> theme.highlightColour
          , paddingLeft: "20px"
          , fontFamily: theme.textFontFamily
          , gridArea: "header"
          , display: "flex"
          , alignItems: "center"
          , width: "100vw"
          , height: "80px"
          }
      , logo: css { width: "70px", height: "70px", padding: "5px", marginRight: "7px" }
      }
  component "Header" \{} -> React.do
    classes <- useStyles
    pure
      $ R.header
          { className: classes.sidebar
          , children:
            [ R.div { children: [ element pslogoIcon {} ], className: classes.logo }
            , element h { level: H2, text: "Purescript", className: Nothing }
            ]
          }
