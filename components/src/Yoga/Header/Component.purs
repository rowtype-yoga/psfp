module Yoga.Header.Component where

import Prelude
import Color (toHexString)
import Data.Interpolate (i)
import Effect (Effect)
import JSS (jssClasses)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, reactComponent, element)
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
            { background: theme.backgroundColour # toHexString
            , borderBottom: "1px solid " <> (theme.backgroundColourLightest # toHexString)
            , borderRight: "1px solid " <> (theme.backgroundColourLightest # toHexString)
            , borderLeft: "1px solid " <> (theme.backgroundColourLightest # toHexString)
            , color: theme.textColour # toHexString
            , fontSize: "2em"
            , padding: "20px"
            , display: "flex"
            , flex: "0 0 100%"
            , boxShadow:
              (i "30px 30px 60px " (toHexString theme.backgroundColourDarker) ", -30px -30px 60px " (toHexString theme.backgroundColourLighter) ";") ∷ String
            }
          , logo:
            { fill: theme.textColour # toHexString
            , marginRight: "10px"
            , marginBottom: "5px"
            }
          }
  reactComponent "Header" \{ kids, className } -> React.do
    classNames <- useStyles {}
    pure
      $ R.div
          { className: classNames.card <> " " <> className
          , children:
            [ element trianglelogoIcon { width: "36", className: classNames.logo } ]
              <> kids
          }
