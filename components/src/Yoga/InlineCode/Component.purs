module Yoga.InlineCode.Component where

import Prelude
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe)
import Data.Nullable (toNullable)
import Data.Nullable as Nullable
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Foreign.Object as Obj
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, component, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Record.Extra (pick)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Helpers ((?||))
import Yoga.InlineCode.Styles (styles)
import Yoga.InlineCode.Styles as Styles
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps (Styles.PropsR)

type OptionalProps r
  = ( update ∷ String -> Effect Unit
    , text ∷ Maybe String
    , className ∷ Maybe String
    , readOnly ∷ Maybe Boolean
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS styles
  component "InlineCode" \props@{ className, text, update, readOnly } -> React.do
    value /\ modifyValue <- useState (text ?|| "")
    classes <- useStyles $ pick props
    useAff value do
      delay (200.0 # Milliseconds)
      update value # liftEffect
    pure
      $ R.div
          { className: classes.form
          , children:
            [ R.input
                { className: intercalate " " [ classes.inlinecode, fold className ]
                , maxLength: props.width ?|| 10
                , value
                , readOnly: readOnly ?|| false
                , disabled: readOnly ?|| false
                , spellCheck: false
                , autoComplete: unsafeCoerce "false"
                , autoCorrect: "off"
                , autoCapitalize: "off"
                , onChange:
                  handler targetValue
                    ( \v -> modifyValue $ const (v ?|| "")
                    )
                , _data: Obj.singleton "testid" "inline-code"
                }
            ]
          }
