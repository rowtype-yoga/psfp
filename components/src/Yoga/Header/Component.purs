module Yoga.Header.Component where

import Prelude
import Data.Interpolate (i)
import Effect (Effect)
import JSS (jss, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import Yoga.SVG.Icon (trianglelogoIcon)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

mkHeader ∷
  Effect
    ( ReactComponent
        { kids ∷ Array JSX
        , className ∷ String
        }
    )
mkHeader = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { card:
            jss
              { background: theme.backgroundColour
              , borderBottom: "1px solid " <> theme.backgroundColourLightest
              , borderRight: "1px solid " <> theme.backgroundColourLightest
              , borderLeft: "1px solid " <> theme.backgroundColourLightest
              , color: theme.textColour
              , fontFamily: theme.textFontFamily
              , fontSize: "2em"
              , padding: "20px"
              , display: "flex"
              , flex: "0 0 100%"
              , boxShadow:
                (i "30px 30px 60px " theme.backgroundColourDarker ", -30px -30px 60px " theme.backgroundColourLighter ";") ∷ String
              }
          , logo:
            jss
              { fill: theme.textColour
              , marginRight: "10px"
              , marginBottom: "5px"
              }
          }
  component "Header" \{ kids, className } -> React.do
    classNames <- useStyles {}
    pure
      $ R.div
          { className: classNames.card <> " " <> className
          , children:
            [ element trianglelogoIcon { width: "36", className: classNames.logo } ]
              <> kids
          }
