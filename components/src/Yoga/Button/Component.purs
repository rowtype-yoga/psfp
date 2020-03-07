module Yoga.Button.Component where

import Prelude
import CSS (ColorSpace(..), mix, toHexString)
import Yoga.CSS.Safer (cssSafer)
import Color (fromHexString)
import Data.Foldable (intercalate)
import Data.Interpolate (i)
import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import Effect (Effect)
import Prim.Row (class Lacks, class Union)
import React.Basic (JSX)
import React.Basic.DOM (Props_button, unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import Record (union)
import Yoga.Theme (increaseContrast)
import Yoga.Theme.Styles (classNames, makeStyles)
import Yoga.Theme.Types (CSSTheme)

data ButtonType
  = PlainButton
  | HighlightedButton
  | DisabledButton

derive instance eqButtonType ∷ Eq ButtonType

mkButton ∷
  ∀ attrs attrs_.
  Union attrs attrs_ Props_button =>
  Lacks "children" attrs =>
  Lacks "ref" attrs =>
  Lacks "key" attrs =>
  Effect
    ( ReactComponent
        { kids ∷ Array JSX
        , buttonType ∷ ButtonType
        , buttonProps ∷ Record attrs
        , className ∷ String
        }
    )
mkButton = do
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      { buttonContainer:
        cssSafer
          { padding: "2px"
          , background: linearGradient [ "145deg", theme.highlightColour, theme.highlightColourDark ]
          , borderRadius: "20px"
          , height: "40px"
          , minWidth: "100px"
          , boxSizing: "border-box"
          , margin: "2px"
          }
      , "@keyframes gradientBG":
        cssSafer
          { "0%": { backgroundPosition: "0% 50%" }
          , "50%": { backgroundPosition: "100% 50%" }
          , "100%": { backgroundPosition: "0% 50%" }
          }
      , btn:
        cssSafer
          { background:
            linearGradient
              [ "145deg"
              , theme.interfaceColourLightest
              , theme.interfaceColourDarkest
              ]
          , color: theme.highlightColour --theme.textColour
          , borderRadius: "20px"
          , border: "0"
          , width: "100%"
          , height: "36px"
          , fontFamily: theme.textFontFamily
          , padding: "1px 18px 0px 18px"
          , letterSpacing: "0.2em"
          , textTransform: "uppercase"
          , outline: "none"
          , "&:focus":
            { background:
              linearGradient
                [ "145deg"
                , theme.interfaceColourDarker
                , theme.highlightColourDark
                , theme.highlightColour
                ]
            , color:
              fromMaybe theme.textColour do
                hlc <- theme.highlightColour # fromHexString
                hlcd <- theme.highlightColourDark # fromHexString
                let
                  bg = if theme.isLight then hlc else mix HSL hlc hlcd 0.5
                tc <- theme.textColour # fromHexString
                pure $ increaseContrast bg tc # toHexString
            , backgroundSize: "400% 400%, 100% 100%"
            , animation: "$gradientBG 3s ease infinite"
            }
          , "&:active":
            { boxShadow:
              ( i "inset 6px 6px 6px "
                  theme.backgroundColourDarkest
                  ", inset -6px -6px 6px "
                  theme.backgroundColourLightest ::
                  String
              )
            , background:
              linearGradient
                [ "145deg"
                , theme.backgroundColourDarker
                , theme.backgroundColourLighter
                ]
            }
          , "&:disabled":
            { boxShadow: "0 0 0 black"
            , background: theme.backgroundColour
            , border: "1px dotted " <> theme.interfaceColourLightest
            , textDecoration: "line-through"
            , textDecorationColor: theme.red
            }
          }
      , highlightedButton:
        cssSafer
          { background:
            if theme.isLight then
              theme.highlightColour
            else
              linearGradient [ theme.highlightColour, theme.highlightColourDark ]
          , "&:active":
            { background:
              linearGradient [ "145deg", theme.highlightColourDark, theme.highlightColour ]
            }
          , color:
            fromMaybe theme.textColour do
              hlc <- theme.highlightColour # fromHexString
              hlcd <- theme.highlightColourDark # fromHexString
              let
                bg = if theme.isLight then hlc else mix HSL hlc hlcd 0.5
              tc <- theme.textColour # fromHexString
              pure $ increaseContrast bg tc # toHexString
          }
      }
  component "Button" \{ kids, buttonType, buttonProps, className } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    -- [TODO]: How do you do this in a typesafe manner?
    pure
      $ R.div
          { className: rawClasses.buttonContainer
          , children:
            [ (element $ unsafeCreateDOMComponent "button")
                $ { className:
                    ( classes
                        [ _.btn
                        , guard (buttonType == HighlightedButton) _.highlightedButton
                        ]
                    )
                      <> " "
                      <> className
                  , disabled: buttonType == DisabledButton
                  , children: kids
                  }
                    `union`
                      buttonProps
            ]
          }

linearGradient ∷ Array String -> String
linearGradient elems = "linear-gradient(" <> intercalate "," elems <> ")"
