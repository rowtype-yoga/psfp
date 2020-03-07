module Yoga.Header.Component where

import Prelude
import Yoga.CSS.Safer (cssSafer)
import Data.Interpolate (i)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import Yoga.SVG.Icon (trianglelogoIcon)
import Yoga.Theme.Styles (classNames, makeStyles)
import Yoga.Theme.Types (CSSTheme)

mkHeader ∷
  Effect
    ( ReactComponent
        { kids :: Array JSX
        , className :: String
        }
    )
mkHeader = do
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      { card:
        cssSafer
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
            (i "30px 30px 60px " theme.backgroundColourDarker ", -30px -30px 60px " theme.backgroundColourLighter ";") :: String
          }
      , logo:
        cssSafer
          { fill: theme.textColour
          , marginRight: "10px"
          , marginBottom: "5px"
          }
      }
  component "Header" \{ kids, className } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.div
          { className: classes [ _.card ] <> " " <> className
          , children:
            [ element trianglelogoIcon { width: 36, className: rawClasses.logo } ]
              <> kids
          }
