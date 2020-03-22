module Yoga.Theme.CSSBaseline where

import Prelude hiding (add)
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jss, jssClasses)
import React.Basic (ReactComponent)
import React.Basic.Hooks (JSX, component, fragment)
import React.Basic.Hooks as React
import Yoga.Font.Rubik as Rubik
import Yoga.Font.VictorMono as VictorMono
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (YogaTheme)

mkCssBaseline ∷
  Effect (ReactComponent { kids ∷ Array JSX })
mkCssBaseline = do
  useStyles <- makeStylesJSS styles
  component "CSSBaseline" \{ kids } -> React.do
    classes <- useStyles {}
    pure
      $ fragment kids

html ∷ JSSElem {}
html =
  jss
    { "WebkitFontSmoothing": "antialiased" -- Antialiasing. 
    , "MozOsxFontSmoothing": "grayscale" -- Antialiasing. 
    , boxSizing: "border-box"
    }

root ∷ JSSElem {}
root =
  jss
    { "--ratio": "1.5"
    , "--s-5": "calc(var(--s-4) / var(--ratio))"
    , "--s-4": "calc(var(--s-3) / var(--ratio))"
    , "--s-3": "calc(var(--s-2) / var(--ratio))"
    , "--s-2": "calc(var(--s-1) / var(--ratio))"
    , "--s-1": "calc(var(--s0) / var(--ratio))"
    , "--s0": "1rem"
    , "--s1": "calc(var(--s0) * var(--ratio))"
    , "--s2": "calc(var(--s1) * var(--ratio))"
    , "--s3": "calc(var(--s2) * var(--ratio))"
    , "--s4": "calc(var(--s3) * var(--ratio))"
    , "--s5": "calc(var(--s4) * var(--ratio))"
    }

styles ∷ JSSClasses YogaTheme {} ( "@global" ∷ JSSElem {} )
styles =
  jssClasses \theme ->
    { "@global":
      jss
        { html
        , "@font-face": jss (Rubik.fontFamilies <> VictorMono.fontFamilies)
        , ":root": root
        , "*":
          { maxWidth: theme.measure
          }
        , "*, *::before, *::after":
          { boxSizing: "inherit"
          , fontFamily: "inherit"
          , color: "inherit"
          , overflowWrap: "break-word"
          , margin: 0
          , padding: 0
          }
        , "strong, b": { fontWeight: theme.fontWeightBold }
        , "html, body, button, div, header, nav, main, footer":
          { maxWidth: "none"
          }
        , body:
          { margin: 0
          , backgroundColor: theme.backgroundColour
          , color: theme.textColour
          , fontFamily: theme.textFontFamily
          , "&::backdrop":
            { backgroundColor: theme.backgroundColour
            }
          }
        , "h1, h2, h3, h4":
          { "line-height": "calc(0.8 * var(--ratio))" -- [TODO]: Move to variable
          , "font-weight": 700
          , "hyphens": "auto"
          }
        , "h1, .h1":
          { "font-size": "var(--s4)"
          }
        , "h2, .h2":
          { "font-size": "var(--s3)"
          }
        , "h3, .h3":
          { "font-size": "var(--s2)"
          }
        , "h4, .h4":
          { "font-size": "var(--s1)"
          }
        }
    }
