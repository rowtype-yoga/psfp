module Yoga.Theme.CSSBaseline where

import Prelude hiding (add)
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jss, jssClasses)
import React.Basic (ReactComponent)
import React.Basic.Hooks (JSX, component, fragment)
import React.Basic.Hooks as React
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
        , ":root": root
        , "*":
          { maxWidth: theme.measure
          }
        , "*, *::before, *::after":
          { boxSizing: "inherit"
          }
        , "strong, b": { fontWeight: theme.fontWeightBold }
        , "html, body, div, header, nav, main, footer":
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
        }
    }
