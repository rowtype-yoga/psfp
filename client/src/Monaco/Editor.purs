module Editor where

import Prelude

import CSS.Safer (cssSafer)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Nullable (Nullable, notNull)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Foreign (Foreign, unsafeToForeign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element, fragment)
import React.Basic.Hooks (Ref, component, writeRef)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import React.Helpers (wrapperDiv)
import Theme.Styles (makeStyles, useTheme)
import Theme.Types (CSSTheme)
import Web.DOM (Node)

type EditorProps
  = ( value ∷ String
    , language ∷ String
    , editorDidMount ∷ EffectFn2 Node Editor Unit
    , theme ∷ String
    , line ∷ Number
    , width ∷ String
    , height ∷ String
    , loading ∷ JSX
    , options ∷ Foreign
    )

foreign import editorImpl ∷ ∀ attrs. ReactComponent { | attrs }

foreign import initMonacoImpl ∷ Effect (Promise Monaco)

foreign import defineThemeImpl ∷ Monaco -> String -> MonacoTheme -> Effect Unit

foreign import setThemeImpl ∷ Monaco -> String -> Effect Unit

foreign import nightOwlTheme ∷ MonacoTheme

foreign import vsCodeTheme ∷ MonacoTheme

foreign import getValue ∷ Editor -> Effect String

foreign import data Monaco ∷ Type

foreign import data Editor ∷ Type

foreign import data MonacoTheme ∷ Type

editor ∷ ∀ attrs attrs_. Union attrs attrs_ EditorProps => ReactComponent { | attrs }
editor = editorImpl

initMonaco ∷ Aff Monaco
initMonaco = liftEffect initMonacoImpl >>= Promise.toAff

darkThemeName ∷ String
darkThemeName = "NightOwl"

lightThemeName ∷ String
lightThemeName = "VSCode"

foreign import data MonarchLanguage ∷ Type

foreign import purescriptSyntax ∷ MonarchLanguage

foreign import registerLanguageImpl ∷ Monaco -> String -> Effect Unit

foreign import setMonarchTokensProviderImpl ∷ Monaco -> String -> MonarchLanguage -> Effect Unit

initEditor ∷ Aff Unit
initEditor = do
  monaco <- initMonaco
  defineThemeImpl monaco darkThemeName nightOwlTheme # liftEffect
  defineThemeImpl monaco lightThemeName vsCodeTheme # liftEffect
  registerLanguageImpl monaco "purescript" # liftEffect
  setMonarchTokensProviderImpl monaco "purescript" purescriptSyntax # liftEffect

mkEditor ∷ Effect (ReactComponent { editorRef ∷ Ref (Nullable _) })
mkEditor = do
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      { wrapper:
        cssSafer
          { margin: "0"
          , boxSizing: "border-box"
          , padding: "35px 40px"
          , width: "100%"
          , height: "100%"
          , borderRadius: "32px"
          , backgroundColor: theme.backgroundColour
          }
      }
  component "Editor" \{ editorRef } -> React.do
    classes <- useStyles
    useAff unit initEditor
    theme <- useTheme
    let themeName = if theme.isLight then lightThemeName else darkThemeName
    pure
      $ fragment
          [ wrapperDiv { className: classes.wrapper }
              $ element editor
                  { theme: themeName
                  , options:
                    unsafeToForeign
                      { fontFamily: "PragmataPro"
                      , fontLigatures: true
                      , fontSize: "16pt"
                      , lineNumbers: "off"
                      , glyphMargin: false
                      , folding: false
                      , lineDecorationsWidth: 0
                      , lineNumbersMinChars: 0
                      , minimap: { enabled: false }
                      }
                  , language: "purescript"
                  -- https://microsoft.github.io/monaco-editor/playground.html#extending-language-services-custom-languages
                  , editorDidMount:
                    mkEffectFn2 \_ -> notNull >>> writeRef editorRef
                  }
          ]
