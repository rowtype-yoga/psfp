module Yoga.Box.Component where

import Prelude
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Box.Styles (styles)
import Yoga.Box.Styles as Styles
import Yoga.Helpers (ifJustTrue)
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps (Styles.PropsR)

type OptionalProps r
  = ( kids ∷ Array JSX
    , className ∷ Maybe String
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS styles
  component "Box" \props@{ kids, className, padding } -> React.do
    classes <- useStyles (pick props)
    pure
      $ R.div
          { className: intercalate " " [ classes.box, ifJustTrue props.invert classes.invert, fold className ]
          , children: kids
          , style: (unsafeCoerce props).style -- Hack for animated components :(
          }
