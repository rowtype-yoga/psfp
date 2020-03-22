module Yoga.Button.Component where

import Prelude
import CSS (ColorSpace(..), hotpink, mix, toHexString)
import CSS as Color
import Color (fromHexString)
import Data.Foldable (intercalate)
import Data.Interpolate (i)
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (guard)
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Yoga.Helpers ((?||))
import Yoga.Theme (increaseContrast, unsafeAlaColor)
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
            Color.cssStringRGBA do
              let
                col = Color.fromHexString theme.backgroundColour ?|| hotpink

                { r, g, b, a } = Color.toRGBA (col)
              Color.rgba r g b 0.2
          , "&:active":
            { background:
              linearGradient [ "145deg", theme.highlightColourDark, theme.highlightColour ]
            }
          , color:
            fromMaybe theme.textColour do
              hlc <- theme.highlightColour # fromHexString
              hlcd <- theme.highlightColourDark # fromHexString
              text <- theme.textColour # fromHexString
              tc <- pure $ if Color.isLight hlcd then text else Color.complementary text
              -- pure $ increaseContrast hlcd Color.white # toHexString
              pure $ Color.white # toHexString
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
          { padding: "2px"
          , background: linearGradient [ "135deg", theme.highlightColourRotatedBackwards, theme.highlightColourRotatedForwards ]
          , borderRadius: "var(--s1)"
          , height: "calc(var(--s1) - var(--s1-3))"
          , minWidth: "var(--s2)"
          , boxSizing: "border-box"
          }
        , "@keyframes gradientBG":
          { "0%": { backgroundPosition: "0% 50%" }
          , "50%": { backgroundPosition: "100% 50%" }
          , "100%": { backgroundPosition: "0% 50%" }
          }
        , btn:
          { width: "100%"
          , background: linearGradient [ "180deg", theme.backgroundColourLightest, theme.backgroundColourDarkest ]
          , color:
            theme.highlightColour
              # unsafeAlaColor \c -> if Color.isLight c then Color.darken 0.2 c else Color.lighten 0.2 c
          , borderRadius: "var(--s1)"
          , border: "0"
          , height: "var(--s2)"
          , fontSize: "var(--s-1)"
          , fontFamily: theme.textFontFamily
          , padding: "0 var(--s0) 0 var(--s0)"
          , letterSpacing: "var(--s-5)"
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
            , backgroundSize: "200% 400%, 100% 100%"
            , animation: "$gradientBG 3s ease infinite"
            }
          , "&:active":
            { boxShadow:
              ( i "inset 6px 6px 6px "
                  theme.backgroundColourDarkest
                  ", inset -6px -6px 6px "
                  theme.backgroundColourLightest ∷
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

linearGradient ∷ Array String -> String
linearGradient elems = "linear-gradient(" <> intercalate "," elems <> ")"
