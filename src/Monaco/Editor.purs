module Editor where

import Prelude
import CSS.Safer (cssSafer)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (warn)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Foreign (Foreign, unsafeToForeign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element, fragment)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, readRefMaybe, useRef, writeRef)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import React.Helpers (wrapperDiv)
import Theme.Styles (makeStyles_)
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

foreign import getValue ∷ Editor -> Effect String

foreign import data Monaco ∷ Type

foreign import data Editor ∷ Type

foreign import data MonacoTheme ∷ Type

editor ∷
  ∀ attrs attrs_. Union attrs attrs_ EditorProps => ReactComponent { | attrs }
editor = editorImpl

initMonaco ∷ Aff Monaco
initMonaco = liftEffect initMonacoImpl >>= Promise.toAff

themeName ∷ String
themeName = "NightOwl"

foreign import data MonarchLanguage ∷ Type

foreign import purescriptSyntax ∷ MonarchLanguage

foreign import registerLanguageImpl ∷ Monaco -> String -> Effect Unit

foreign import setMonarchTokensProviderImpl ∷ Monaco -> String -> MonarchLanguage -> Effect Unit

mkEditor ∷ Effect (ReactComponent {})
mkEditor = do
  useStyles <-
    makeStyles_
      { wrapper:
        cssSafer
          { margin: "0"
          , boxSizing: "border-box"
          , padding: "35px 40px"
          , width: "100%"
          , height: "100%"
          , borderRadius: "32px"
          , backgroundColor: "#011627"
          }
      }
  component "Editor" \{} -> React.do
    classes <- useStyles
    ref <- useRef null
    useAff unit
      $ do
          monaco <- initMonaco
          defineThemeImpl monaco themeName nightOwlTheme # liftEffect
          setThemeImpl monaco themeName # liftEffect
          registerLanguageImpl monaco "purescript" # liftEffect
          setMonarchTokensProviderImpl monaco "purescript" purescriptSyntax # liftEffect
    pure
      $ fragment
          [ R.button
              { onClick:
                handler_ do
                  maybeEditor <- readRefMaybe ref
                  case maybeEditor of
                    Nothing -> pure unit
                    Just ed -> do
                      v <- getValue ed
                      warn $ "Value: " <> v
              }
          , wrapperDiv { className: classes.wrapper }
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
                    mkEffectFn2 \_ -> notNull >>> writeRef ref
                  }
          ]
