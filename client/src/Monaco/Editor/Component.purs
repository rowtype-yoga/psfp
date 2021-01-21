module Editor.Component where

import Prelude
import CSS.Safer (cssSafer)
import Data.Nullable (Nullable, notNull)
import Editor (defineThemeImpl, editor, initMonaco, nightOwlTheme, purescriptSyntax, registerLanguageImpl, setMonarchTokensProviderImpl, vsCodeTheme)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn2)
import Foreign (unsafeToForeign)
import React.Basic (ReactComponent, element, fragment)
import React.Basic.Hooks (Ref, component, writeRef)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import React.Helpers (wrapperDiv)
import Theme.Styles (makeStyles, useTheme)
import Theme.Types (CSSTheme)

darkThemeName ∷ String
darkThemeName = "NightOwl"

lightThemeName ∷ String
lightThemeName = "VSCode"

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
    let
      themeName = if theme.isLight then lightThemeName else darkThemeName
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
