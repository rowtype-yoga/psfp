module Yoga.CompileEditor.Component where

import Prelude hiding (add)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.TwoOrMore (twoOrMore)
import Effect (Effect)
import Effect.Aff (Error, launchAff_, message, try)
import Effect.Class (liftEffect)
import Framer.Motion as M
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (handler_)
import React.Basic.Hooks (reactComponent, useState)
import React.Basic.Hooks as React
import Shared.Models.Body (CompileResult, RunResult)
import Yoga ((</>))
import Yoga as Y
import Yoga.Block as Block
import Yoga.Block.Atom.Button.Types as BT
import Yoga.Block.Container.Style (colour, getDarkOrLightMode)
import Yoga.Compiler.Types (Compiler)
import Yoga.Editor (getValue, mkEditor)

type Props =
  { initialCode ∷ String, height ∷ String, language ∷ String }

mkCompileEditor ∷ ∀ r. { | Compiler r } -> Effect (ReactComponent Props)
mkCompileEditor { compileAndRun } = do
  editor <- mkEditor
  motionBox <- M.custom Block.box
  motionBox1 <- M.custom Block.box
  motionButton <- M.custom Block.button
  reactComponent "CompileEditor" \{ initialCode, height, language } -> React.do
    activeIndex /\ updateActiveIndex <- React.useState' 0
    mbEditor /\ modifyEditor <- React.useState Nothing
    themeMode /\ setThemeMode <- React.useState' Nothing
    value /\ setValue <- React.useState' initialCode
    let onChange newValue event = setValue newValue
    React.useEffectAlways do
      mode <- getDarkOrLightMode
      unless (mode == themeMode) do setThemeMode mode
      mempty
    let
      onLoad e = do
        modifyEditor (const $ Just e)
    compileResult /\ modifyCompileResult <- useState Nothing
    let
      showEditor = updateActiveIndex 0
      showResult = updateActiveIndex 1
      reset = resetCompileResult *> showEditor
      run = compile *> showResult
      resetCompileResult = do
        setCompileResult Nothing
        setValue initialCode
      compileResultToString ∷ Maybe (Either Error (Either CompileResult RunResult)) -> String
      compileResultToString = case _ of
        Nothing -> ""
        Just (Right (Left cr)) -> cr.result <#> _.message # intercalate "\n"
        Just (Right (Right r)) -> r.stdout
        Just (Left e) -> message e
      setCompileResult = modifyCompileResult <<< const
      compile = do
        for_ mbEditor \ed -> do
          setCompileResult Nothing
          code <- getValue ed
          launchAff_ do
            res <- try $ compileAndRun { code }
            setCompileResult (Just res) # liftEffect
      tabs ∷ JSX
      tabs =
        Block.segmented
          </> { activeIndex
            , updateActiveIndex
            , buttonContents:
              twoOrMore
                { id: "editor", value: "Editor" }
                { id: "result", value: "Result" }
                []
            }
      editorView ∷ JSX
      editorView = editor </> { onLoad, height, language, value, onChange }
      buttons ∷ Array JSX -> JSX
      buttons = Y.el Block.cluster { justify: "flex-end", space: "var(--s-1)" }
      button =
        Y.el motionButton
          <<< M.motion
              { transition: M.transition $ { duration: 0.2 }
              }
      resetButton ∷ JSX
      resetButton =
        button
          { buttonType: BT.Generic, onClick: handler_ reset }
          [ R.text "Reset" ]
      runButton ∷ JSX
      runButton =
        button
          { buttonType: BT.Primary, onClick: handler_ run }
          [ R.text "Run" ]
      buttonControls ∷ Array JSX -> JSX
      buttonControls = Y.el motionBox $ M.motion { layout: M.layout true } {}
      tabContainer ∷ Array JSX -> JSX
      tabContainer children =
        Y.styled R.div'
          { className: "tabby"
          , css:
            E.css
              { background: E.str colour.backgroundLayer3
              , borderTop: E.str $ "1px solid " <> colour.interfaceBackgroundHighlight
              , borderRadius: E.str "14px 14px 0 0"
              , paddingLeft: E.var "--s-1"
              , paddingRight: E.var "--s-1"
              , paddingTop: E.str "0"
              , paddingBottom: E.str "0"
              , marginBottom: E.str "0"
              , zIndex: E.str "0"
              }
          }
          [ Y.el Block.centre { andText: true } children
          ]
      terminal ∷ Array JSX -> JSX
      terminal =
        Y.el motionBox1
          $ M.motion
              { layout: M.layout true
              }
              { css:
                E.css
                  { background: E.str colour.backgroundLayer2
                  , boxSizing: E.str "content-box"
                  , margin: E.str "0"
                  , minHeight: E.str "8em"
                  , height: E.str height
                  , padding: E.str "0"
                  , display: E.str "flex"
                  }
              }
      container =
        Y.el motionBox1
          $ M.motion { layout: M.layout true }
              { css:
                E.css
                  { background: E.str colour.backgroundLayer3
                  , borderBottom: E.str $ "1px solid " <> colour.interfaceBackgroundShadow
                  , borderRadius: E.str "0 0 14px 14px"
                  , padding: E.str "0"
                  , zIndex: E.str "0"
                  }
              }
    pure
      $ Y.el Block.stack { space: E.str "0" }
          [ tabContainer [ tabs ]
          , Y.el M.animateSharedLayout {}
              [ container
                  [ terminal
                      [ if activeIndex == 0 then
                          editorView
                        else
                          Y.el R.div'
                            { style: css { width: "100%" }
                            }
                            [ R.text (compileResultToString compileResult) ]
                      ]
                  , buttonControls
                      [ buttons [ resetButton, runButton ]
                      ]
                  ]
              ]
          ]
