module Card.Component where

import Prelude

import Data.Foldable (intercalate)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Theme.Styles (classNames, makeStyles)
import Theme.Types (Theme)

mkCard ∷
  Effect
    ( ReactComponent
        { children ∷ Array JSX
        }
    )
mkCard = do
  useStyles <-
    makeStyles \(theme ∷ Theme) ->
      { card:
        css
          { backgroundImage:
            "linear-gradient("
              <> intercalate ","
                  [ "46deg"
                  , theme.backgroundColourLighter
                  , theme.backgroundColourLight
                  ]
              <> ")"
          , color: theme.foregroundColour
          , boxShadow: "1px 1px 20px rgba(0,0,0,0.66)"
          , borderRadius: "5px"
          , padding: "36px 40px 32px 40px"
          }
      }
  component "Card" \{ children } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.div
          { className: classes [ _.card ]
          , children
          }

mkCardTitle ∷
  Effect
    ( ReactComponent
        { children ∷ Array JSX
        }
    )
mkCardTitle = do
  useStyles <-
    makeStyles \(theme ∷ Theme) ->
      { cardtitle:
        css
          { color: theme.foregroundColourLighter
          , fontSize: "1.2em"
          , fontWeight: "400"
          , fontFamily: theme.headingFontFamily
          , padding: "4px 8px"
          }
      }
  component "CardTitle" \{ children } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.div
          { className: classes [ _.cardtitle ]
          , children
          }

mkCardSubtitle ∷
  Effect
    ( ReactComponent
        { children ∷ Array JSX
        }
    )
mkCardSubtitle = do
  useStyles <-
    makeStyles \(theme ∷ Theme) ->
      { cardtitle:
        css
          { color: theme.foregroundColourDarker
          , opacity: "0.8"
          , fontSize: "1.0em"
          , fontWeight: "500"
          , fontFamily: theme.headingFontFamily
          , padding: "4px 8px 22px 8px"
          }
      }
  component "CardSubtitle" \{ children } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.div
          { className: classes [ _.cardtitle ]
          , children
          }

mkCardContent ∷
  Effect
    ( ReactComponent
        { children ∷ Array JSX
        }
    )
mkCardContent = do
  useStyles <-
    makeStyles \(theme ∷ Theme) ->
      { cardContent:
        css
          { fontFamily: theme.textFontFamily
          , color: theme.foregroundColourDark
          , fontSize: "0.8em"
          , fontWeight: "300"
          , "WebkitFontSmoothing": "subpixel-antialiased"
          , textAlign: "justify"
          , hyphens: "auto"
          , padding: "4px 8px 22px 8px"
          }
      }
  component "CardContent" \{ children } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.div
          { className: classes [ _.cardContent ]
          , children
          }
