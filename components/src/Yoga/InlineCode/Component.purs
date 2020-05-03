module Yoga.InlineCode.Component where

import Prelude
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Foreign.Object as Obj
import Literals.Undefined (undefined)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, component, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Record.Extra (pick)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.DOM.Hook (useFocus)
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
    , debounceBy ∷ Maybe Milliseconds
    , focusOnFirstRender ∷ Maybe Boolean
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS styles
  component "InlineCode" \props@{ text, update } -> React.do
    value /\ modifyValue <- useState (text ?|| "")
    classes <- useStyles $ pick props
    ref <- useFocus
    useAff value do
      delay $ props.debounceBy ?|| (16.667 # Milliseconds)
      update value # liftEffect
    pure
      $ R.div
          { className: classes.container
          , children:
            [ R.input
                { className: intercalate " " [ classes.inlinecode, fold props.className ]
                , maxLength: props.width ?|| 10
                , value
                , ref: if props.focusOnFirstRender == Just true then ref else unsafeCoerce undefined
                , readOnly: props.readOnly ?|| false
                , disabled: props.readOnly ?|| false
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
