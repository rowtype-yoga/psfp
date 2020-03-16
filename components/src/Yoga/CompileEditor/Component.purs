module Yoga.CompileEditor.Component where

import Prelude hiding (add)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, attempt, error, launchAff_, message, throwError)
import Effect.Class (liftEffect)
import JSS (jss, jssClasses)
import Milkis as M
import Milkis.Impl (FetchImpl)
import React.Basic (ReactComponent)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, element, fragment, useState)
import React.Basic.Hooks as React
import Shared.Json (readAff)
import Shared.Models.Body as Body
import Simple.JSON (writeJSON)
import Yoga.Button.Component (ButtonType(..), mkButton)
import Yoga.Card.Component (mkCard)
import Yoga.Editor (getValue, mkEditor, setValue)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

type Props
  = { initialCode ∷ String, height ∷ String, language ∷ String }

mkCompileEditor ∷ FetchImpl -> Effect (ReactComponent Props)
mkCompileEditor fetch = do
  editor <- mkEditor
  card <- mkCard
  button <- mkButton
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { editor:
            jss
              { background: theme.backgroundColour
              , boxSizing: "content-box"
              , height: "80%"
              , padding: "20px"
              , marginTop: "0px"
              , borderRadius: "12px"
              , boxShadow: i "22px 22px 24px " theme.backgroundColourDarker ", -22px -22px 24px " theme.backgroundColourLighter ∷ String
              , display: "flex"
              , flexDirection: "column"
              }
          , buttons:
            jss
              { marginBottom: "4px"
              , display: "flex"
              , alignSelf: "flex-end"
              }
          , compileButton: jss {}
          , resetButton: jss {}
          , card:
            jss
              { marginLeft: "35px"
              , marginRight: "35px"
              , opacity: 0
              , zIndex: 0
              }
          , cardHidden: jss { opacity: 0 }
          , compileError: jss { color: theme.red, opacity: 1, transition: "opacity 2.0s ease" }
          , runOutput: jss { color: theme.green, opacity: 1, transition: "opacity 2.0s ease" }
          }
  component "CompileEditor" \{ initialCode, height, language } -> React.do
    maybeEditor /\ modifyEditor <- useState Nothing
    classes <- useStyles {}
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
            res <- compileAndRun (M.fetch fetch) { code }
            setCompileResult (Just res) # liftEffect
    pure
      $ fragment
          [ R.div
              { children:
                [ R.div
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
                , element editor { onLoad, height, language }
                ]
              , className: classes.editor
              }
          , element card
              { kids: [ R.text (compileResultToString compileResult) ]
              , className: classes.card <> " " <> compileResultToClass compileResult
              }
          ]

compileAndRun ∷ M.Fetch -> Body.CompileRequest -> Aff (Either Body.CompileResult Body.RunResult)
compileAndRun fetch body = do
  response <-
    attempt
      $ fetch (M.URL "/api/compileAndRun")
          { method: M.postMethod
          , body: writeJSON body
          , headers: M.makeHeaders { "Content-Type": "application/json" }
          }
  case response of
    Left l ->
      pure
        ( Left
            { resultType: ""
            , result:
              [ { allSpans: []
                , errorCode: ""
                , errorLink: ""
                , filename: ""
                , message: message l
                , moduleName: Nothing
                , position:
                  { endColumn: 0
                  , endLine: 0
                  , startColumn: 0
                  , startLine: 0
                  }
                , suggestion: Nothing
                }
              ]
            }
        )
    Right r -> case M.statusCode r of
      200 -> M.json r >>= readAff <#> Right
      422 -> M.json r >>= readAff <#> Left
      code -> throwError (error $ "Unexpected response code " <> show code)
