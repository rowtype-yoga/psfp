module Yoga.Card.Component where

import Prelude
import Color (cssStringRGBA)
import Color as Color
import Data.Foldable (fold)
import Data.Interpolate (i)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Effect (Effect)
import JSS (JSSClasses, JSSElem, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM (CSS)
import React.Basic.DOM as R
import React.Basic.Helpers (orUndefined)
import React.Basic.Hooks (ReactComponent, Ref, reactComponent)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Web.DOM (Node)
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
        , borderTop: "1px solid rgba(255,255,255,0.1)"
        , borderBottom: "1px solid rgba(0,0,0,0.1)"
        , boxShadow:
          i -- bottom right
            "var(--s-5) var(--s-3) var(--s-3) "
            (cssStringRGBA $ withAlpha 0.1 Color.black)
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

type PropsR r
  = ( kids ∷ Array JSX
    , className ∷ Maybe String
    , style ∷ Maybe CSS
    , divRef ∷ Maybe (Ref (Nullable Node))
    | r
    )

type Props
  = { | PropsR (StyleProps) }

mkCard ∷ Effect (ReactComponent Props)
mkCard = do
  useStyles <- makeStylesJSS styles
  box <- Box.makeComponent
  reactComponent "Card" \(props@{ kids, style, className } ∷ Props) -> React.do
    classes <- useStyles (pick props)
    pure
      $ R.div
          { ref: props.divRef # orUndefined
          , className: classes.card <> " " <> fold className
          , style: style # orUndefined
          , children: kids
          }
