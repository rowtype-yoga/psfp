module Yoga.InlineCode.Component where

import Prelude
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe)
import Data.String (trim)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign.Object as Obj
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, component, useState)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Yoga.Helpers ((?||))
import Yoga.InlineCode.Styles (styles)
import Yoga.InlineCode.Styles as Styles
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps (Styles.PropsR)

data Action
  = CompileAndRunCode String

derive instance eqAction ∷ Eq Action
type OptionalProps r
  = ( dispatch ∷ Action -> Effect Unit
    , className ∷ Maybe String
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS styles
  component "InlineCode" \props@{ className, dispatch } -> React.do
    value /\ modifyValue <- useState ""
    classes <- useStyles $ pick props
    pure
      $ R.form
          { className: classes.form
          , children:
            [ R.input
                { className: intercalate " " [ classes.inlinecode, fold className ]
                , maxLength: props.width ?|| 10
                , value
                , onChange:
                  handler targetValue
                    ( \v -> modifyValue $ const (v ?|| "")
                    )
                , _data: Obj.singleton "testid" "inline-code"
                }
            ]
          , onSubmit:
            handler preventDefault
              (const $ dispatch $ CompileAndRunCode value)
          }
