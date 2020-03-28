module Yoga.Card.Component where

import Prelude
import Color (cssStringRGBA, toHexString)
import Color as Color
import Data.Interpolate (i)
import Data.Maybe (Maybe)
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Yoga.Box.Component as Box
import Yoga.Helpers ((?||))
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme, YogaTheme)

type StyleProps
  = {}

styles ∷ JSSClasses YogaTheme StyleProps ( card ∷ JSSElem StyleProps )
styles =
  jssClasses \(theme ∷ CSSTheme) ->
    let
      lighten x = Color.lighten if theme.isLight then (0.5 * x) else 1.7 * x

      darken x = Color.darken if theme.isLight then 1.0 * x else (1.2 * x)
    in
      { card:
        { borderRadius: "var(--s1)"
        , boxShadow:
          ( i -- bottom right
              "var(--s1) var(--s1) var(--s2) "
              (cssStringRGBA $ darken 0.04 theme.backgroundColour)
              --bottom left
              ", calc(-1 * var(--s1)) var(--s1) var(--s2) "
              (cssStringRGBA $ darken 0.01 theme.backgroundColour)
              --top left
              ", calc(-1 * var(--s1)) calc(-1 * var(--s1)) var(--s2) "
              (cssStringRGBA $ lighten 0.05 theme.backgroundColour)
              --top right
              ", var(--s1) calc(-1 * var(--s1)) var(--s2) "
              (cssStringRGBA $ darken 0.02 theme.backgroundColour)
          ) ∷
            String
        -- background: (i "linear-gradient(145deg," (toHexString theme.backgroundColourDarker) ", " (toHexString theme.backgroundColourLighter) ")") ∷ String
        }
      }

mkCard ∷
  Effect
    ( ReactComponent
        { kids ∷ Array JSX
        , className ∷ Maybe String
        }
    )
mkCard = do
  useStyles <- makeStylesJSS styles
  box <- Box.makeComponent
  component "Card" \{ kids, className } -> React.do
    classes <- useStyles {}
    pure
      $ jsx box
          { className: classes.card <> " " <> (className ?|| "") }
          kids
