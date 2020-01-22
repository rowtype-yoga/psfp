module CompileEditor.Component where

import Prelude hiding (add)

import Button.Component (ButtonType(..), mkButton)
import Card.Component (mkCard)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Editor (getValue, mkEditor)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import React.Basic (ReactComponent)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, element, fragment, readRefMaybe, useRef, useState)
import React.Basic.Hooks as React
import Shared.Json (readAff)
import Shared.Models.Body as Body
import Simple.JSON (writeJSON)

mkCompileEditor ∷ Effect (ReactComponent {})
mkCompileEditor = do
  editor <- mkEditor
  card <- mkCard
  button <- mkButton
  component "StorybookEditor" \{} -> React.do
    editorRef <- useRef null
    compileResult /\ modifyCompileResult <- useState ""
    let
      setCompileResult = modifyCompileResult <<< const
    pure
      $ fragment
          [ element button
              { buttonType: PlainButton
              , children: [ R.text "Compile" ]
              , buttonProps:
                { onClick:
                  handler_ do
                    maybeEditor <- (readRefMaybe editorRef)
                    for_ maybeEditor \ed -> do
                      code <- getValue ed
                      launchAff_ do
                        res <- compileAndRun { code }
                        liftEffect $ setCompileResult case res of 
                          Left cr -> (cr.result.errors <#> _.message # intercalate "/n")
                          Right r -> r.stdout
                }
              }
          , element card { children: [ R.text compileResult ] }
          , R.div { children: [ element editor { editorRef } ], style: css { height: "100%" } }
          ]

fetch ∷ M.Fetch
fetch = M.fetch windowFetch

compileAndRun :: Body.CompileRequest -> Aff (Either Body.CompileResult Body.RunResult)
compileAndRun body = do
  response <-
    fetch (M.URL "/api/compileAndRun")
      { method: M.postMethod
      , body: writeJSON body
      , headers: M.makeHeaders { "Content-Type": "application/json" }
      }
  case M.statusCode response of
    200 -> M.json response >>= readAff <#> Left
    400 -> M.json response >>= readAff <#> Right 
    code -> throwError (error $ "Unexpected response code " <> show code)