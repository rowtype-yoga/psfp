module CompileEditor.Component where

import Prelude hiding (add)
import Button.Component (ButtonType(..), mkButton)
import CSS.Safer (cssSafer)
import Card.Component (mkCard)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Editor (getValue, mkEditor, setValue)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import React.Basic (ReactComponent)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, element, fragment, useState)
import React.Basic.Hooks as React
import Shared.Json (readAff)
import Shared.Models.Body as Body
import Simple.JSON (writeJSON)
import Theme.Styles (makeStyles)
import Theme.Types (CSSTheme)

type Props
  = { initialCode :: String, height :: String }

mkCompileEditor ∷ Effect (ReactComponent Props)
mkCompileEditor = do
  editor <- mkEditor
  card <- mkCard
  button <- mkButton
  useStyles <-
    makeStyles \(theme :: CSSTheme) ->
      { editor:
        cssSafer
          { background: theme.codeBackgroundColour
          , height: "80%"
          , padding: "20px"
          , margin: "20px"
          , marginLeft: "35px"
          , marginRight: "35px"
          , borderRadius: "12px"
          }
      , buttons: cssSafer { float: "right", marginRight: "40px", height: "30px" }
      , compileButton: cssSafer {}
      , resetButton: cssSafer {}
      , card:
        cssSafer
          { marginTop: "70px"
          , marginLeft: "35px"
          , marginRight: "35px"
          }
      , cardHidden: cssSafer { opacity: 0 }
      , compileError: cssSafer { color: theme.red, animation: "0.7s shake ease" }
      , runOutput: cssSafer { color: theme.green, animation: "0.4s bounceIn ease" }
      }
  component "StorybookEditor" \{ initialCode, height } -> React.do
    maybeEditor /\ modifyEditor <- useState Nothing
    classes <- useStyles
    let
      onLoad e = do
        setValue initialCode e
        modifyEditor (const $ Just e)
    compileResult /\ modifyCompileResult <- useState Nothing
    let
      reset = do
        setCompileResult Nothing
        for_ maybeEditor (setValue initialCode)

      compileResultToString = case _ of
        Nothing -> ""
        Just (Left cr) -> cr.result <#> _.message # intercalate "/n"
        Just (Right r) -> r.stdout

      compileResultToClass = case _ of
        Nothing -> classes.cardHidden
        Just (Left cr) -> classes.compileError
        Just (Right r) -> classes.runOutput

      setCompileResult = modifyCompileResult <<< const

      compile = do
        for_ maybeEditor \ed -> do
          setCompileResult Nothing
          code <- getValue ed
          launchAff_ do
            res <- compileAndRun { code }
            setCompileResult (Just res) # liftEffect
    pure
      $ fragment
          [ R.div
              { children: [ element editor { onLoad, height } ]
              , className: classes.editor
              }
          , R.div
              { className: classes.buttons
              , children:
                [ element button
                    { buttonType: PlainButton
                    , kids: [ R.text "Reset" ]
                    , buttonProps:
                      { onClick: handler_ reset
                      }
                    , className: classes.resetButton
                    }
                , element button
                    { buttonType: HighlightedButton
                    , kids: [ R.text "Compile" ]
                    , buttonProps:
                      { onClick: handler_ compile
                      }
                    , className: classes.compileButton
                    }
                ]
              }
          , element card
              { kids: [ R.text (compileResultToString compileResult) ]
              , className: classes.card <> " " <> compileResultToClass compileResult
              }
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
    200 -> M.json response >>= readAff <#> Right
    422 -> M.json response >>= readAff <#> Left
    code -> throwError (error $ "Unexpected response code " <> show code)
