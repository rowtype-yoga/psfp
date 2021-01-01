module Yoga.Button.Component where

import Prelude
import CSS (Color, ColorSpace(..), cssStringRGBA)
import CSS as Color
import Data.Array.NonEmpty as NEA
import Data.Foldable (intercalate)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jssClasses)
import Prim.Row (class Nub, class Union)
import React.Basic (JSX)
import React.Basic.DOM (Props_button)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (ReactComponent, reactComponent)
import React.Basic.Hooks as React
import Record as Record
import Yoga.Helpers ((?||))
import Yoga.Theme (withAlpha)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme, YogaTheme)

data ButtonType
  = PlainButton
  | HighlightedButton
  | DisabledButton

derive instance eqButtonType ∷ Eq ButtonType

type StyleProps =
  {}

highlightStyles ∷
  JSSClasses YogaTheme {}
    ( highlightedButton ∷ JSSElem {}
    )
highlightStyles =
  jssClasses
    $ \(theme ∷ CSSTheme) ->
        { highlightedButton:
          { background:
            linearGradient
              "180deg"
              [ withAlpha (if theme.isLight then 0.2 else 0.2) (Color.lighten 0.5 theme.highlightColour)
              , withAlpha (if theme.isLight then 0.15 else 0.4) (Color.darken 0.5 theme.highlightColour)
              ]
              <> " ,"
              <> linearGradient
                  "225deg"
                  [ theme.highlightColourRotatedBackwards
                  , theme.highlightColourRotatedForwards
                  ]
          , color: Color.cssStringRGBA theme.backgroundColour <> " !important"
          , transition: "box-shadow 8.0 ease-in-out"
          , boxShadow:
            "0 var(--s-5) var(--s-2)" <> (Color.cssStringRGBA $ withAlpha 0.2 Color.black)
          , backgroundClip: "border-box"
          , "-webkit-background-clip": "border-box"
          , textFillColor: theme.backgroundColour
          , "-webkit-text-fill-color": theme.backgroundColour
          , "&:active":
            { boxShadow:
              i "inset var(--s-3) var(--s-3) var(--s-3) "
                (cssStringRGBA $ Color.darken 0.5 theme.backgroundColourDarker)
                ", inset calc(-1 * var(--s-3)) calc(-1 * -var(--s-3)) var(--s-3) "
                (cssStringRGBA $ Color.darken 0.2 theme.backgroundColourDarkest) ∷
                String
            , color: Color.darken 0.05 theme.backgroundColour
            , textFillColor: Color.darken 0.05 theme.backgroundColour
            , background:
              linearGradient
                "0deg"
                [ withAlpha (if theme.isLight then 0.2 else 0.2) (Color.darken 0.1 theme.highlightColour)
                , withAlpha (if theme.isLight then 0.15 else 0.4) (Color.darken 0.8 theme.highlightColour)
                ]
                <> " ,"
                <> linearGradient
                    "225deg"
                    [ theme.highlightColourRotatedBackwards
                    , theme.highlightColourRotatedForwards
                    ]
            }
          }
        }

styles ∷
  JSSClasses YogaTheme {}
    ( btn ∷ JSSElem StyleProps
    , disabled ∷ JSSElem StyleProps
    , container ∷ JSSElem StyleProps
    )
styles =
  jssClasses
    $ \(theme ∷ CSSTheme) ->
        let
          darken x = Color.darken x
          lighten x = Color.lighten x
          more amount x = if Color.isLight x then lighten amount x else darken amount x
          less amount x = if Color.isLight x then darken amount x else lighten amount x
        in
          { container:
            \props ->
              { borderRadius: "var(--s1)"
              , "-webkit-background-clip": "border-box"
              , backgroundClip: "border-box"
              , background:
                linearGradient (if theme.isLight then "180deg" else "0deg")
                  [ (if theme.isLight then Color.lighten 0.01 <<< Color.desaturate 0.43 else darken 0.16) $ more 0.08 $ Color.mix HSL theme.backgroundColour theme.highlightColourRotatedBackwards 0.05 # (flip (Color.mix HSL theme.highlightColourRotatedForwards) (if theme.isLight then 0.94 else 0.85))
                  , (if theme.isLight then Color.lighten 0.02 <<< Color.desaturate 0.43 else darken 0.16) $ less 0.05 $ Color.mix HSL theme.backgroundColour theme.highlightColourRotatedForwards 0.05 # (flip (Color.mix HSL theme.highlightColourRotatedBackwards) (if theme.isLight then 0.84 else 0.70))
                  ]
              , display: "inline-block"
              , transition: "all 0.2s ease-in-out"
              }
          , btn:
            { background:
              linearGradient "127deg"
                [ (if theme.isLight then darken 0.1 else identity) $ theme.highlightColourRotatedBackwards
                , (if theme.isLight then darken 0.1 else identity) $ theme.highlightColourRotatedForwards
                ]
            , transition: "all 0.2s ease-in-out"
            , boxShadow:
              "0 var(--s-5) var(--s-2)" <> (Color.cssStringRGBA $ withAlpha 0.1 Color.black)
            , padding: "calc(var(--s0) - var(--s-4)) var(--s1) calc(var(--s0) - var(--s-5)) var(--s1)"
            , backgroundClip: "text"
            , "-webkit-background-clip": "text"
            , textFillColor: "transparent"
            , "-webkit-text-fill-color": "transparent"
            , borderRadius: "var(--s1)"
            , border: "0"
            -- , height: "calc(var(--s2) + var(--s-2))"
            , fontSize: "calc(var(--s-2) + var(--s-3))"
            , fontFamily: NEA.head theme.textFontFamily
            , fontWeight: "500"
            , letterSpacing: "var(--s-4)"
            , textTransform: "uppercase"
            , outline: "none"
            , "&:focus":
              { boxShadow:
                i "var(--s-4) 0 var(--s-2)"
                  (cssStringRGBA $ withAlpha 0.5 theme.highlightColourRotatedForwards)
                  ", calc(-1 * var(--s-4)) 0 var(--s-2)"
                  (cssStringRGBA $ withAlpha 0.5 theme.highlightColourRotatedForwards)
                  ", 0 calc(-1 * var(--s-4)) var(--s-2)"
                  (cssStringRGBA $ withAlpha 0.5 theme.highlightColourRotatedBackwards)
                  ", 0 var(--s-4) var(--s-2)"
                  (cssStringRGBA $ withAlpha 0.5 theme.highlightColourRotatedBackwards) ∷
                  String
              }
            , "&:active":
              { boxShadow:
                i "inset var(--s-3) var(--s-3) var(--s-3) "
                  (cssStringRGBA $ darken 0.5 theme.backgroundColourDarker)
                  ", inset calc(-1 * var(--s-3)) calc(-1 * -var(--s-3)) var(--s-3) "
                  (cssStringRGBA $ darken 0.2 theme.backgroundColourDarkest)
                  ", var(--s-4) 0 var(--s-2)"
                  (cssStringRGBA $ withAlpha 0.5 theme.highlightColourRotatedForwards)
                  ", calc(-1 * var(--s-4)) 0 var(--s-2)"
                  (cssStringRGBA $ withAlpha 0.5 theme.highlightColourRotatedForwards)
                  ", 0 calc(-1 * var(--s-4)) var(--s-2)"
                  (cssStringRGBA $ withAlpha 0.5 theme.highlightColourRotatedBackwards)
                  ", 0 var(--s-4) var(--s-2)"
                  (cssStringRGBA $ withAlpha 0.5 theme.highlightColourRotatedBackwards) ∷
                  String
              , background:
                -- linearGradient (if theme.isLight then "0deg" else "180deg")
                --   [ less 0.04 $ withAlpha 0.92 theme.backgroundColour
                --   , less 0.09 $ withAlpha 0.92 theme.backgroundColour
                --   ]
                linearGradient (if theme.isLight then "0deg" else "180deg")
                  $ ( Color.darken 0.1
                        <$> [ (if theme.isLight then Color.lighten 0.01 <<< Color.desaturate 0.43 else darken 0.16) $ more 0.08 $ Color.mix HSL theme.backgroundColour theme.highlightColourRotatedBackwards 0.05 # (flip (Color.mix HSL theme.highlightColourRotatedForwards) (if theme.isLight then 0.94 else 0.85))
                          , (if theme.isLight then Color.lighten 0.02 <<< Color.desaturate 0.43 else darken 0.16) $ less 0.05 $ Color.mix HSL theme.backgroundColour theme.highlightColourRotatedForwards 0.05 # (flip (Color.mix HSL theme.highlightColourRotatedBackwards) (if theme.isLight then 0.84 else 0.70))
                          ]
                    )
              , color: more 0.02 theme.highlightColour
              , textFillColor: more 0.02 theme.highlightColour
              }
            , "&:disabled":
              { boxShadow: "none"
              , backgroundClip: "border-box"
              , "-webkit-background-clip": "border-box"
              , textFillColor: "initial"
              , "-webkit-text-fill-color": "initial"
              , background: "transparent"
              , color: less 0.2 theme.backgroundColour
              }
            }
          , "disabled":
            { background:
              (cssStringRGBA $ withAlpha 0.5 $ less 0.1 theme.backgroundColour)
                <> " !important"
            , boxShadow: "none !important"
            }
          }

type MyProps =
  ( children ∷ Array JSX, className ∷ String, disabled ∷ Boolean, onClick ∷ EventHandler )

type Props r =
  { kids ∷ Array JSX
  , buttonType ∷ Maybe ButtonType
  , onClick ∷ EventHandler
  , className ∷ Maybe String
  , buttonProps ∷ Maybe { | r }
  }

mkButton ∷ Effect (ReactComponent (Props ()))
mkButton = mkButtonWithProps

type WithProps r =
  Effect (ReactComponent (Props r))

mkButtonWithProps ∷
  ∀ extra given missing.
  Union extra MyProps given =>
  Nub given given =>
  Union given missing Props_button =>
  Effect (ReactComponent (Props extra))
mkButtonWithProps = do
  useBaseStyles <- makeStylesJSS styles
  useHighlightStyles <- makeStylesJSS highlightStyles
  reactComponent "Button" \(props@{ kids, onClick } ∷ Props extra) -> React.do
    classes <- useBaseStyles {}
    { highlightedButton } <- useHighlightStyles {}
    let
      buttonType = props.buttonType ?|| PlainButton
      className = props.className ?|| ""
      buttonProps ∷ { | MyProps }
      buttonProps =
        { className:
          intercalate " "
            [ classes.btn
            , guard (buttonType == HighlightedButton) highlightedButton
            , className
            ]
        , disabled: buttonType == DisabledButton
        , onClick
        , children: kids
        }
    pure
      $ R.div
          { className: classes.container <> " " <> if buttonType == DisabledButton then classes.disabled else ""
          , children:
            [ case props.buttonProps of
                Just bps -> R.button $ Record.merge bps buttonProps
                Nothing -> R.button buttonProps
            ]
          }

linearGradient ∷ String -> Array Color -> String
linearGradient direction elems = "linear-gradient(" <> direction <> "," <> intercalate "," (map cssStringRGBA elems) <> ")"
