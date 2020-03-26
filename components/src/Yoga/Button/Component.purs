module Yoga.Button.Component where

import Prelude
import CSS (Color, ColorSpace(..), mix, toHexString)
import CSS as Color
import Data.Foldable (intercalate)
import Data.Interpolate (i)
import Data.Maybe (Maybe)
import Data.Monoid (guard)
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Yoga.Helpers ((?||))
import Yoga.Theme (increaseContrast, unsafeWithAlpha)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme, YogaTheme)

data ButtonType
  = PlainButton
  | HighlightedButton
  | DisabledButton

derive instance eqButtonType ∷ Eq ButtonType
type StyleProps
  = {}

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
              ( if theme.isLight then
                  "180deg"
                else
                  "0deg"
              )
              [ do
                  let
                    col = theme.highlightColourRotatedBackwards

                    { r, g, b, a } = Color.toRGBA (Color.saturate (10.0) col)
                  Color.rgba r g b 0.5
              , do
                  let
                    col = theme.highlightColourRotatedForwards

                    { r, g, b, a } = Color.toRGBA (Color.saturate (10.0) col)
                  Color.rgba r g b 0.5
              ]
          , "&:active":
            { background:
              linearGradient "145deg" [ theme.highlightColourDark, theme.highlightColour ]
            }
          , color: Color.white
          }
        }

styles ∷
  JSSClasses YogaTheme {}
    ( "@keyframes gradientBG" ∷ JSSElem StyleProps
    , btn ∷ JSSElem StyleProps
    , buttonContainer ∷ JSSElem StyleProps
    )
styles =
  jssClasses
    $ \(theme ∷ CSSTheme) ->
        { buttonContainer:
          { padding: "var(--s-4)"
          , background:
            linearGradient "135deg"
              [ theme.highlightColourRotatedBackwards
              , theme.highlightColourRotatedForwards
              ]
          , borderRadius: "var(--s1)"
          , height: "calc(var(--s1) - var(--s1-3))"
          , minWidth: "var(--s2)"
          , boxSizing: "border-box"
          , boxShadow:
            ( unsafeWithAlpha 0.15 "#000000"
            )
              <> " 0px 1px 2px 2px"
          }
        , "@keyframes gradientBG":
          { "0%": { backgroundPosition: "0% 50%" }
          , "50%": { backgroundPosition: "100% 50%" }
          , "100%": { backgroundPosition: "0% 50%" }
          }
        , btn:
          { width: "100%"
          , background:
            linearGradient "180deg"
              [ Color.darken 0.05 theme.backgroundColour
              , Color.darken 0.10 theme.backgroundColour
              ]
          , color:
            let
              c = theme.highlightColour
            in
              if Color.isLight c then Color.rotateHue (-10.0) c else Color.lighten 0.2 c
          , borderRadius: "var(--s1)"
          , border: "0"
          , height: "calc(var(--s2) + var(--s-4) - 2px)"
          , fontSize: "calc(var(--s-1))"
          , fontFamily: "Rubik Regular"
          , fontWeight: "600"
          , padding: "0 var(--s0) 0 var(--s0)"
          , letterSpacing: "var(--s-5)"
          , textTransform: "uppercase"
          , outline: "none"
          , "&:focus":
            { background:
              linearGradient
                "145deg"
                [ theme.interfaceColourDarker
                , theme.highlightColourDark
                , theme.highlightColour
                ]
            , color:
              do
                let
                  hlc = theme.highlightColour

                  hlcd = theme.highlightColourDark

                  bg = if theme.isLight then hlc else mix HSL hlc hlcd 0.5

                  tc = theme.textColour
                increaseContrast bg tc
            , backgroundSize: "200% 400%, 100% 100%"
            , animation: "$gradientBG 3s ease infinite"
            }
          , "&:active":
            { boxShadow:
              ( i "inset 6px 6px 6px "
                  (toHexString theme.backgroundColourDarkest)
                  ", inset -6px -6px 6px "
                  (toHexString theme.backgroundColourLightest)
              ) ∷
                String
            , background:
              linearGradient
                "145deg"
                [ theme.backgroundColourDarker
                , theme.backgroundColourLighter
                ]
            }
          , "&:disabled":
            { boxShadow: "0 0 0 black"
            , background: theme.grey
            , border: "1px dotted " <> toHexString theme.interfaceColourLightest
            , textDecoration: "line-through"
            , textDecorationColor: theme.red
            }
          }
        }

type Props
  = { kids ∷ Array JSX
    , buttonType ∷ Maybe ButtonType
    , onClick ∷ EventHandler
    , className ∷ Maybe String
    }

mkButton ∷ Effect (ReactComponent Props)
mkButton = do
  useBaseStyles <- makeStylesJSS styles
  useHighlightStyles <- makeStylesJSS highlightStyles
  component "Button" \props@{ kids, onClick } -> React.do
    classes <- useBaseStyles {}
    { highlightedButton } <- useHighlightStyles {}
    let
      buttonType = props.buttonType ?|| PlainButton

      className = props.className ?|| ""
    pure
      $ R.div
          { className: classes.buttonContainer
          , children:
            [ R.button
                $ { className:
                    intercalate " "
                      [ classes.btn
                      , guard (buttonType == HighlightedButton) highlightedButton
                      , className
                      ]
                  , disabled: buttonType == DisabledButton
                  , onClick
                  , children: kids
                  }
            ]
          }

linearGradient ∷ String -> Array Color -> String
linearGradient direction elems = "linear-gradient(" <> direction <> "," <> intercalate "," (map toHexString elems) <> ")"
