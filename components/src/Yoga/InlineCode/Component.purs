module Yoga.InlineCode.Component where

import Prelude
import Prelude
import CSS (value)
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe)
import Data.String (trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Foreign.Object as Obj
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, capture_, preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (ReactComponent, component, useEffect, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Record.Extra (pick)
import Yoga.Helpers ((?||))
import Yoga.InlineCode.Styles (styles)
import Yoga.InlineCode.Styles as Styles
import Yoga.Theme.Styles (makeStylesJSS)

type Props
  = Record PropsR

type PropsR
  = OptionalProps (Styles.PropsR)

type OptionalProps r
  = ( onSubmit ∷ String -> Effect Unit
    , className ∷ Maybe String
    | r
    )

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  useStyles <- makeStylesJSS styles
  component "InlineCode" \props@{ className, onSubmit } -> React.do
    value /\ modifyValue <- useState ""
    classes <- useStyles $ pick props
    useAff value do
      delay (200.0 # Milliseconds)
      onSubmit value # liftEffect
    pure
      $ R.div
          { className: classes.form
          , children:
            [ R.input
                { className: intercalate " " [ classes.inlinecode, fold className ]
                , maxLength: props.width ?|| 10
                , value
                , spellCheck: false
                , autoComplete: false
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
