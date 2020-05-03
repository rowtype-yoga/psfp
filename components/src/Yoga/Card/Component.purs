module Yoga.Card.Component where

import Prelude
import Color (cssStringRGBA)
import Color as Color
import Data.Foldable (fold)
import Data.Interpolate (i)
import Data.Maybe (Maybe)
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Box.Component as Box
import Yoga.Helpers (ifJustTrue)
import Yoga.Theme (withAlpha)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme, YogaTheme)

type StyleProps
  = ( zoomOnHover ∷ Maybe Boolean )

styles ∷ JSSClasses YogaTheme { | StyleProps } ( card ∷ JSSElem { | StyleProps } )
styles =
  jssClasses \(theme ∷ CSSTheme) ->
    { card:
      \props ->
        { borderRadius: "var(--s-2)"
        , overflow: "hidden"
        , boxShadow:
          i -- bottom right
            "var(--s-5) var(--s-3) var(--s-3) "
            (cssStringRGBA $ withAlpha 0.13 Color.black)
            ", calc(var(--s-5) * -1) calc(-1 * var(--s-5)) var(--s-2) "
            (cssStringRGBA $ withAlpha 0.1 Color.black) ∷
            String
        , transform: ifJustTrue props.zoomOnHover $ "scale3d(0.9, 0.9, 1.0)"
        , transition: "transform 0.2s ease-in-out"
        , "&:hover":
          { transform: "scale3d(1.0, 1.0, 1.0)"
          }
        }
    }

type Props r
  = ( kids ∷ Array JSX
    , className ∷ Maybe String
    | r
    )

mkCard ∷
  Effect
    ( ReactComponent
        { | Props (StyleProps) }
    )
mkCard = do
  useStyles <- makeStylesJSS styles
  box <- Box.makeComponent
  component "Card" \props@{ kids, className } -> React.do
    classes <- useStyles (pick props)
    pure
      $ R.div
          { className: classes.card <> " " <> fold className
          , children: kids
          }
