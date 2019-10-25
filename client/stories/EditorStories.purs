module EditorStories where

import Prelude hiding (add)

import Data.Foldable (for_)
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Decorator.FullScreen (fullScreenDecorator)
import Editor (getValue, mkEditor)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import React.Basic (ReactComponent)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, element, fragment, readRefMaybe, useRef, useState)
import React.Basic.Hooks as React
import Shared.Json (readAff)
import Shared.Models.Body as Body
import Simple.JSON (writeJSON)
import Storybook.React (Storybook, add, addDecorator, storiesOf)

stories ∷ Effect Storybook
stories = do
  storiesOf "Editor" do
    addDecorator fullScreenDecorator
    add "The Editor" mkCompileEditor
      [ {}
      ]

mkCompileEditor ∷ Effect (ReactComponent {})
mkCompileEditor = do
  editor <- mkEditor
  component "StorybookEditor" \{} -> React.do
    editorRef <- useRef null
    compileResult /\ modifyCompileResult <- useState ""
    let
      setCompileResult = modifyCompileResult <<< const
    pure
      $ fragment
          [ R.button
              { children: pure $ R.text "Compile"
              , onClick:
                handler_ do
                  maybeEditor <- (readRefMaybe editorRef)
                  for_ maybeEditor \ed -> do
                    code <- getValue ed
                    launchAff_ do
                      res <- compile { code }
                      res2 <- run
                      setCompileResult res2.stdout # liftEffect


              }
          , R.textarea
              { value: compileResult
              , disabled: true
              }
          , element editor { editorRef }
          ]

fetch ∷ M.Fetch
fetch = M.fetch windowFetch

compile ∷ Body.CompileRequest -> Aff Body.CompileResult
compile body = do
  fetch (M.URL "/api/compile")
    { method: M.postMethod
    , body: spy "Body" $ writeJSON body
    , headers: M.makeHeaders { "Content-Type": "application/json" }
    }
    >>= M.json
    >>= readAff

run ∷ Aff Body.RunResult
run = do
  fetch (M.URL "/api/run")
    { method: M.postMethod }
    >>= M.json
    >>= readAff
