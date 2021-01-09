module Yoga.Editor where

import Prelude
import CSS (borderBox, borderRadius, boxSizing, margin, padding, pct, px, unitless, width)
import CSS.Overflow (hidden, overflowY)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Foldable (fold, foldMap, for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Nullable (Nullable, notNull, null)
import Data.Time.Duration (Seconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn1)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, mkEffectFn1, mkEffectFn2)
import Foreign (Foreign, unsafeToForeign)
import JSS (jssClasses)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, Ref, element, fragment)
import React.Basic.DOM as R
import React.Basic.Hooks (reactComponent, useEffect, useEffectAlways, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Web.DOM (Node)
import Web.HTML (HTMLElement)
import Yoga.Block.Container.Style (DarkOrLightMode(..), getDarkOrLightMode)
import Yoga.Block.Hook.UseResize (useOnResize)
import Yoga.Theme.Styles (makeStylesJSS)

type EditorProps =
  ( value ∷ String
  , language ∷ String
  , editorDidMount ∷ EffectFn2 Editor Node Unit
  , editorWillMount ∷ EffectFn1 Monaco Unit
  , theme ∷ String
  , line ∷ Number
  , ref ∷ Ref (Nullable HTMLElement)
  , width ∷ String
  , height ∷ String
  , loading ∷ JSX
  , options ∷ Foreign
  )

foreign import monacoEditorImpl ∷ ∀ attrs. Effect (Promise (ReactComponent { | attrs }))

foreign import defineThemeImpl ∷ Monaco -> String -> MonacoTheme -> Effect Unit

foreign import setThemeImpl ∷ Monaco -> String -> Effect Unit

foreign import nightOwlTheme ∷ String -> MonacoTheme

foreign import horizonTheme ∷ String -> MonacoTheme

foreign import getValue ∷ Editor -> Effect String

foreign import setValue ∷ String -> Editor -> Effect Unit

foreign import colorizeImpl ∷ String -> String -> { tabSize ∷ Int } -> Editor -> Promise String

foreign import layout ∷ Editor -> Effect Unit

foreign import data Monaco ∷ Type

foreign import data Editor ∷ Type

foreign import data MonacoTheme ∷ Type

monacoEditor ∷ ∀ attrs attrs_. Union attrs attrs_ EditorProps => Aff (ReactComponent { | attrs })
monacoEditor = do
  prom <- monacoEditorImpl # liftEffect
  Promise.toAff prom

darkThemeName ∷ String
darkThemeName = "NightOwl"

lightThemeName ∷ String
lightThemeName = "Horizon"

foreign import data MonarchLanguage ∷ Type

foreign import purescriptSyntax ∷ MonarchLanguage

foreign import registerLanguageImpl ∷ Monaco -> String -> Effect Unit

foreign import setMonarchTokensProviderImpl ∷ Monaco -> String -> MonarchLanguage -> Effect Unit

initEditor ∷ Monaco -> Effect Unit
initEditor monaco = do
  defineThemeImpl monaco darkThemeName (nightOwlTheme "#212134") -- [TODO] Read from somewhere else
  defineThemeImpl monaco lightThemeName (horizonTheme "#EBDFDE") -- [TODO] Don't hardcode
  registerLanguageImpl monaco "purescript"
  setMonarchTokensProviderImpl monaco "purescript" purescriptSyntax

type Props =
  { onLoad ∷ Editor -> Effect Unit
  , height ∷ String
  , language ∷ String
  }

mkEditor ∷ Effect (ReactComponent Props)
mkEditor = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \_ ->
          { wrapper:
            do
              margin (0.0 # unitless) (0.0 # unitless) (0.0 # unitless) (0.0 # unitless)
              boxSizing borderBox
              borderRadius (8.0 # px) (8.0 # px) (8.0 # px) (8.0 # px)
              padding (4.0 # px) (8.0 # px) (0.0 # px) (8.0 # px)
              width (100.0 # pct)
              overflowY hidden
          -- backgroundColor (colour.background) [ TODO ]
          }
  reactComponent "Editor" \{ onLoad, height, language } -> React.do
    classes <- useStyles {}
    mbEditorComponent /\ setEditorComponent <- React.useState' Nothing
    monacoRef <- React.useRef null
    editorRef <- React.useRef null
    useOnResize (3.0 # Seconds) \_ -> do
      mbEditor <- React.readRefMaybe editorRef
      for_ mbEditor layout
    useAff unit do
      med <- monacoEditor
      setEditorComponent (Just med) # liftEffect
    useEffectAlways do
      mbMode <- getDarkOrLightMode
      mbMonaco <- React.readRefMaybe monacoRef
      case mbMode, mbMonaco of
        Just DarkMode, Just monaco -> setThemeImpl monaco darkThemeName
        Just LightMode, Just monaco -> setThemeImpl monaco lightThemeName
        _, _ -> mempty
      mempty
    let themeName = darkThemeName
    pure
      $ fragment
          [ R.div
              { className: classes.wrapper
              , children:
                [ mbEditorComponent
                    # foldMap \editor ->
                        element editor
                          { theme: themeName
                          , height
                          , options:
                            unsafeToForeign
                              { fontFamily: "Victor Mono"
                              , fontLigatures: true
                              , fontSize: "15px"
                              , lineNumbers: "off"
                              , glyphMargin: false
                              , folding: false
                              , lineDecorationsWidth: 0
                              , lineNumbersMinChars: 0
                              , minimap: { enabled: false }
                              , scrollBeyondLastLine: false
                              , hideCursorInOverviewRuler: true
                              , overviewRulerBorder: false
                              , renderLineHighlightOnlyWhenFocus: true
                              , scrollbar: { alwaysConsumeMouseWheel: false }
                              , renderLineHighlight: "none"
                              }
                          , language
                          -- https://microsoft.github.io/monaco-editor/playground.html#extending-language-services-custom-languages
                          , editorDidMount:
                            mkEffectFn2 \e _ -> do
                              React.writeRef editorRef (notNull e)
                              onLoad e
                          , editorWillMount:
                            mkEffectFn1 \m -> do
                              React.writeRef monacoRef (notNull m)
                              initEditor m
                          }
                ]
              }
          ]
