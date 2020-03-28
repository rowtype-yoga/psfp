module Yoga.Button.Component where

import Prelude
import CSS (Color, cssStringRGBA, desaturate)
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
import Yoga.Theme (withAlpha)
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
              "135deg"
              [ withAlpha (if theme.isLight then 0.15 else 0.07) theme.backgroundColourDarker
              , withAlpha (if theme.isLight then 0.15 else 0.07) theme.backgroundColourLighter
              ]
          , "&:active":
            { background:
              linearGradient "145deg" [ theme.highlightColourDark, theme.highlightColour ]
            }
          , color: theme.backgroundColour
          }
        }

styles ∷
  JSSClasses YogaTheme {}
    ( "@keyframes gradientBG" ∷ JSSElem StyleProps
    , btn ∷ JSSElem StyleProps
    , buttonContainer ∷ JSSElem StyleProps
    , disabled ∷ JSSElem StyleProps
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
          { buttonContainer:
            { padding: "var(--s-4)"
            , background:
              linearGradient "225deg"
                [ theme.highlightColourRotatedBackwards
                , theme.highlightColourRotatedForwards
                ]
            , borderRadius: "var(--s1)"
            , height: "calc(var(--s1) - var(--s1-3))"
            , minWidth: "var(--s2)"
            , boxSizing: "border-box"
            , boxShadow:
              ( cssStringRGBA (Color.rgba 0 0 0 0.15)
              )
                <> " 0 var(--s-5) var(--s-5) var(--s-5)"
            }
          , "@keyframes gradientBG":
            { "0%": { backgroundPosition: "0% 50%" }
            , "50%": { backgroundPosition: "100% 50%" }
            , "100%": { backgroundPosition: "0% 50%" }
            }
          , btn:
            { width: "calc(100%)"
            , background:
              linearGradient "80deg"
                [ less 0.03 theme.backgroundColour
                , less 0.05 theme.backgroundColour
                ]
            , color:
              let
                c = theme.highlightColour
              in
                if Color.isLight c then Color.rotateHue (-10.0) c else Color.lighten 0.2 c
            , borderRadius: "var(--s1)"
            , border: "0"
            , height: "calc(var(--s2))"
            , fontSize: "calc(var(--s-1))"
            , fontFamily: theme.textFontFamily
            , fontWeight: "600"
            , padding: "0 var(--s0) 0 var(--s0)"
            , letterSpacing: "var(--s-5)"
            , textTransform: "uppercase"
            , outline: "none"
            , "&:focus":
              { boxShadow:
                ( i "var(--s-4) 0 var(--s-2)"
                    (cssStringRGBA theme.highlightColourRotatedForwards)
                    ", calc(-1 * var(--s-4)) 0 var(--s-2)"
                    (cssStringRGBA theme.highlightColourRotatedForwards)
                    ", 0 calc(-1 * var(--s-4)) var(--s-2)"
                    (cssStringRGBA theme.highlightColourRotatedBackwards)
                    ", 0 var(--s-4) var(--s-2)"
                    (cssStringRGBA theme.highlightColourRotatedBackwards)
                ) ∷
                  String
              }
            , "&:active":
              { boxShadow:
                ( i "inset var(--s-3) var(--s-3) var(--s-3) "
                    (cssStringRGBA $ darken 0.3 theme.backgroundColourDarker)
                    ", inset calc(-1 * var(--s-3)) calc(-1 * -var(--s-3)) var(--s-3) "
                    (cssStringRGBA theme.backgroundColourDarkest)
                ) ∷
                  String
              , background:
                linearGradient
                  "145deg"
                  [ theme.backgroundColourDarker
                  , theme.backgroundColourDarkest
                  ]
              }
            , "&:disabled":
              { boxShadow: "0 0 0 black"
              , background: less 0.1 theme.backgroundColour
              , color: less 0.2 theme.backgroundColour
              }
            }
          , "disabled":
            { background:
              (cssStringRGBA $ less 0.1 theme.backgroundColour)
                <> " !important"
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
          { className: classes.buttonContainer <> " " <> if buttonType == DisabledButton then classes.disabled else ""
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
linearGradient direction elems = "linear-gradient(" <> direction <> "," <> intercalate "," (map cssStringRGBA elems) <> ")"
