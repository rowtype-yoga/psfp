module Card.Component where

import Prelude
import CSS.Safer (cssSafer)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Theme.Styles (classNames, makeStyles)
import Theme.Types (CSSTheme)

mkCard ∷
  Effect
    ( ReactComponent
        { kids ∷ Array JSX
        , className :: String
        }
    )
mkCard = do
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      { card:
        cssSafer
          { background: theme.interfaceColourLighter
          , color: theme.textColour
          , margin: "20px"
          , boxShadow:
            "1px 1px 20px rgba(0,0,0,0."
              <> (if theme.isLight then "17" else "43")
              <> ")"
          , borderRadius: "5px"
          , padding: "36px 40px 32px 40px"
          }
      }
  component "Card" \{ kids, className } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.div
          { className: classes [ _.card ] <> " " <> className
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
    makeStyles \(theme ∷ CSSTheme) ->
      { cardtitle:
        cssSafer
          { color: theme.textColourLightest
          , fontSize: "1.2em"
          , fontWeight: "400"
          , fontFamily: theme.headingFontFamily
          , padding: "4px 8px"
          }
      }
  component "CardTitle" \{ kids } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.div
          { className: classes [ _.cardtitle ]
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
    makeStyles \(theme ∷ CSSTheme) ->
      { cardtitle:
        cssSafer
          { color: theme.textColourDarker
          , opacity: "0.8"
          , fontSize: "1.0em"
          , fontWeight: "500"
          , fontFamily: theme.headingFontFamily
          , padding: "4px 8px 22px 8px"
          }
      }
  component "CardSubtitle" \{ kids } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.div
          { className: classes [ _.cardtitle ]
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
    makeStyles \(theme ∷ CSSTheme) ->
      { cardContent:
        cssSafer
          { fontFamily: theme.textFontFamily
          , color: theme.textColourDarker
          , fontSize: "0.8em"
          , fontWeight: "300"
          , "WebkitFontSmoothing": "subpixel-antialiased"
          , textAlign: "justify"
          , hyphens: "auto"
          , padding: "4px 8px 22px 8px"
          }
      }
  component "CardContent" \{ kids } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.div
          { className: classes [ _.cardContent ]
          , children: kids
          }
