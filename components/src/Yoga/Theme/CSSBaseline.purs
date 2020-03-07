module Yoga.Theme.CSSBaseline where

import Prelude hiding (add)
import Yoga.CSS.Safer (cssSafer)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import React.Basic (ReactComponent)
import React.Basic.DOM (CSS)
import React.Basic.Hooks (JSX, component, fragment)
import React.Basic.Hooks as React
import Yoga.Theme.Styles (unsafeMakeStyles)
import Yoga.Theme.Types (CSSTheme)
import Unsafe.Coerce (unsafeCoerce)

mkCssBaseline ::
  Effect (ReactComponent { kids :: Array JSX })
mkCssBaseline = do
  useStyles <- runEffectFn1 unsafeMakeStyles styles
  component "CSSBaseline" \{ kids } -> React.do
    classes <- useStyles
    pure
      $ fragment kids

html :: CSS
html =
  cssSafer
    { "WebkitFontSmoothing": "antialiased" -- Antialiasing. 
    , "MozOsxFontSmoothing": "grayscale" -- Antialiasing. 
    , boxSizing: "border-box"
    }

body :: CSSTheme -> CSS
body theme =
  cssSafer
    { color: theme.textColour
    , backgroundColor: theme.backgroundColour
    , "@media print":
      { backgroundColor: theme.white
      }
    }

styles :: CSSTheme -> CSS
styles theme =
  unsafeCoerce
    { "@global":
      { html
      , "*, *::before, *::after":
        { boxSizing: "inherit"
        }
      , "strong, b": { fontWeight: theme.fontWeightBold }
      , body:
        { margin: 0
        , backgroundColor: theme.backgroundColour
        , color: theme.textColour
        , "&::backdrop":
          { backgroundColor: theme.backgroundColour
          }
        }
      }
    , ".textColouredIcon":
      { fill: theme.textColour
      }
    }
