module Button.Component where

import Prelude
import Data.Foldable (intercalate)
import Data.Monoid (guard)
import Effect (Effect)
import Prim.Row (class Lacks, class Union)
import React.Basic (JSX)
import React.Basic.DOM (Props_button, css, unsafeCreateDOMComponent)
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import Record (union)
import Theme.Styles (classNames, makeStyles)
import Theme.Types (Theme)

data ButtonType
  = PlainButton
  | HighlightedButton
  | DisabledButton

derive instance eqButtonType ∷ Eq ButtonType

mkButton ∷
  ∀ attrs attrs_.
  Union attrs attrs_ Props_button =>
  Lacks "children" attrs =>
  Lacks "className" attrs =>
  Lacks "ref" attrs =>
  Lacks "key" attrs =>
  Effect
    ( ReactComponent
        { children ∷ Array JSX
        , buttonType ∷ ButtonType
        , buttonProps ∷ Record attrs
        }
    )
mkButton = do
  useStyles <-
    makeStyles \(theme ∷ Theme) ->
      { "@keyframes gradientBG":
        css
          { "0%": css { backgroundPosition: "0% 50%" }
          , "50%": css { backgroundPosition: "100% 50%" }
          , "100%": css { backgroundPosition: "0% 50%" }
          }
      , btn:
        css
          { background:
            linearGradient
              [ "180deg", theme.backgroundColourLighter, theme.backgroundColourLight ]
          , color: theme.foregroundColourDarker
          , boxShadow: "1px 1px 10px rgba(0,0,0,0.66)"
          , borderRadius: "20px"
          , padding: "0px 18px 0px 18px"
          , marginLeft: "2px"
          , marginRight: "2px"
          , minWidth: "100px"
          , height: "40px"
          , borderWidth: "0px"
          , fontFamily: theme.textFontFamily
          , letterSpacing: "0.2em"
          , textTransform: "uppercase"
          , outline: "none"
          , "&:focus":
            css
              { background:
                linearGradient
                  [ "-5deg"
                  , theme.backgroundColourDark
                  , theme.highlightColourDark
                  , theme.highlightColour
                  ]
              , color: theme.foregroundColourLighter
              , backgroundSize: "400% 400%, 100% 100%"
              , animation: "$gradientBG 3s ease infinite"
              }
          , "&:active":
            css
              { background:
                linearGradient
                  [ "180deg"
                  , theme.backgroundColourLight
                  , theme.backgroundColourLighter
                  ]
              , boxShadow: "inset 0 0 2px black"
              }
          , "&:disabled":
            css
              { boxShadow: "0 0 0 black"
              , background: theme.backgroundColourLight
              , textDecoration: "line-through"
              }
          }
      , highlightedButton:
        css
          { background:
            linearGradient [ theme.highlightColour, theme.highlightColourDark ]
          , "&:active":
            css
              { background:
                linearGradient [ theme.highlightColourDark, theme.highlightColour ]
              }
          , color: theme.foregroundColourLighter
          }
      }
  component "Button" \{ children, buttonType, buttonProps } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    -- [TODO]: How do you do this in a typesafe manner?
    pure <<< (element $ unsafeCreateDOMComponent "button")
      $ { className:
          classes
            [ _.btn
            , guard (buttonType == HighlightedButton) _.highlightedButton
            ]
        , disabled: buttonType == DisabledButton
        , children
        }
          `union`
            buttonProps

linearGradient ∷ Array String -> String
linearGradient elems = "linear-gradient(" <> intercalate "," elems <> ")"
