module Button.Component where

import Prelude
import CSS (ColorSpace(..), mix, toHexString)
import CSS.Safer (cssSafer)
import Color (fromHexString)
import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import Effect (Effect)
import Prim.Row (class Lacks, class Union)
import React.Basic (JSX)
import React.Basic.DOM (Props_button, unsafeCreateDOMComponent)
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import Record (union)
import Theme (increaseContrast)
import Theme.Styles (classNames, makeStyles)
import Theme.Types (CSSTheme)

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
      { "@keyframes gradientBG":
        cssSafer
          { "0%": { backgroundPosition: "0% 50%" }
          , "50%": { backgroundPosition: "100% 50%" }
          , "100%": { backgroundPosition: "0% 50%" }
          }
      , btn:
        cssSafer
          { background:
            if theme.isLight then
              theme.interfaceColourLightest
            else
              linearGradient
                [ theme.interfaceColourLightest
                , theme.interfaceColourLighter
                ]
          , color: theme.textColour
          , boxShadow:
            if theme.isLight then
              "none"
            else
              "1px 1px 10px rgba(0,0,0,0.66)"
          , border:
            if theme.isLight then
              "1px solid " <> theme.interfaceColourLighter
            else
              "none"
          , borderRadius: "20px"
          , padding: "0px 18px 0px 18px"
          , marginLeft: "2px"
          , marginRight: "2px"
          , minWidth: "100px"
          , height: "40px"
          , fontFamily: theme.textFontFamily
          , letterSpacing: "0.2em"
          , textTransform: "uppercase"
          , outline: "none"
          , "&:focus":
            { background:
              linearGradient
                [ "-5deg"
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
            { background:
              linearGradient
                [ "180deg"
                , theme.interfaceColourLighter
                , theme.interfaceColourLightest
                ]
            , boxShadow: "inset 0 0 2px black"
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
              linearGradient [ theme.highlightColourDark, theme.highlightColour ]
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
    pure <<< (element $ unsafeCreateDOMComponent "button")
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

linearGradient ∷ Array String -> String
linearGradient elems = "linear-gradient(" <> intercalate "," elems <> ")"
