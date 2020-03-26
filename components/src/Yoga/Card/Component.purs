module Yoga.Card.Component where

import Prelude
import Color (toHexString)
import Data.Interpolate (i)
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme, YogaTheme)

type StyleProps
  = {}

styles ∷ JSSClasses YogaTheme StyleProps ( card ∷ JSSElem StyleProps )
styles =
  jssClasses \(theme ∷ CSSTheme) ->
    { card:
      { background: (i "linear-gradient(145deg," (toHexString theme.backgroundColourDarker) ", " (toHexString theme.backgroundColourLighter) ")") ∷ String
      , color: theme.textColour # toHexString
      , fontFamily: theme.textFontFamily
      , margin: "20px"
      , boxShadow:
        (i "30px 30px 60px " (toHexString theme.backgroundColourDarker) ", -30px -30px 60px " (toHexString theme.backgroundColourLighter) ";") ∷ String
      , borderRadius: "15px"
      , padding: "36px 40px 32px 40px"
      }
    }

mkCard ∷
  Effect
    ( ReactComponent
        { kids ∷ Array JSX
        , className ∷ String
        }
    )
mkCard = do
  useStyles <- makeStylesJSS styles
  component "Card" \{ kids, className } -> React.do
    classes <- useStyles {}
    pure
      $ R.div
          { className: classes.card <> " " <> className
          , children: kids
          }

mkCardTitle ∷
  Effect
    ( ReactComponent
        { kids ∷ Array JSX
        }
    )
mkCardTitle = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { cardtitle:
            { color: theme.textColourLightest # toHexString
            , fontSize: "1.2em"
            , fontWeight: "400"
            , fontFamily: theme.headingFontFamily
            , padding: "4px 8px toHexString"
            }
          }
  component "CardTitle" \{ kids } -> React.do
    classNames <- useStyles {}
    pure
      $ R.div
          { className: classNames.cardtitle
          , children: kids
          }

mkCardSubtitle ∷
  Effect
    ( ReactComponent
        { kids ∷ Array JSX
        }
    )
mkCardSubtitle = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { cardtitle:
            { color: theme.textColourDarker # toHexString
            , opacity: "0.8"
            , fontSize: "1.0em"
            , fontWeight: "500"
            , fontFamily: theme.headingFontFamily
            , padding: "4px 8px 22px 8px"
            }
          }
  component "CardSubtitle" \{ kids } -> React.do
    classNames <- useStyles {}
    pure
      $ R.div
          { className: classNames.cardtitle
          , children: kids
          }

mkCardContent ∷
  Effect
    ( ReactComponent
        { kids ∷ Array JSX
        }
    )
mkCardContent = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { cardContent:
            { fontFamily: theme.textFontFamily
            , color: theme.textColourDarker # toHexString
            , fontSize: "0.8em"
            , fontWeight: "300"
            , "WebkitFontSmoothing": "subpixel-antialiased"
            , textAlign: "justify"
            , hyphens: "auto"
            , padding: "4px 8px 22px 8px"
            }
          }
  component "CardContent" \{ kids } -> React.do
    classNames <- useStyles {}
    pure
      $ R.div
          { className: classNames.cardContent
          , children: kids
          }
