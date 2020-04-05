module Yoga.Card.Component where

import Prelude
import CSS (angular, deg, linearGradient, pct)
import Color (cssStringRGBA)
import Color as Color
import Data.Interpolate (i)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jssClasses)
import React.Basic (JSX)
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

      normal =
        if theme.isLight then
          darken 0.03 $ theme.backgroundColour
        else
          lighten 0.015 theme.backgroundColour

      dark =
        if theme.isLight then
          darken 0.03 normal
        else
          darken 0.05 normal

      light =
        if theme.isLight then
          lighten 0.10 normal
        else
          lighten 0.05 normal
    in
      { card:
        { borderRadius: "var(--s1)"
        , boxShadow:
          i -- bottom right
            "var(--s-2) var(--s-2) var(--s-1) "
            (cssStringRGBA dark)
            --bottom left
            ", calc(-1 * var(--s-2)) var(--s-2) var(--s-1) "
            (cssStringRGBA normal)
            --top left
            ", calc(-1 * var(--s-2)) calc(-1 * var(--s-2)) var(--s-1) "
            (cssStringRGBA light)
            --top right
            ", var(--s-2) calc(-1 * var(--s-2)) var(--s-1) "
            (cssStringRGBA normal) ∷
            String
        , background:
          linearGradient (angular (-45.0 # deg))
            [ dark /\ (0.0 # pct)
            , light /\ (100.0 # pct)
            ]
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
  component "Card" \props@{ kids, className } -> React.do
    classes <- useStyles {}
    pure
      $ jsx box
          { className: classes.card <> " " <> (className ?|| "")
          }
          kids
