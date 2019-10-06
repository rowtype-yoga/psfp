module Editor where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (warn)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1)
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element, fragment)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, readRefMaybe, useRef, useState, writeRef)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Simple.JSON as Foreign
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

themeName = "NightOwl"

mkEditor ∷ Effect (ReactComponent {})
mkEditor = do
  component "Editor" \{} -> React.do
    ref <- useRef null
    useAff unit
      $ do
          monaco <- initMonaco
          defineThemeImpl monaco themeName nightOwlTheme # liftEffect
          setThemeImpl monaco themeName # liftEffect
    pure
      $ fragment
      [ R.button { onClick: handler_ do
          maybeEditor <- readRefMaybe ref
          case maybeEditor of
            Nothing -> pure unit
            Just ed -> do
              v <- getValue ed
              warn $ "Value: " <> v
      }
      , element editor
          { theme: themeName
          , options:
            Foreign.write
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
          , language: "haskell"
          -- [TODO] :(
          -- https://microsoft.github.io/monaco-editor/playground.html#extending-language-services-custom-languages
          , editorDidMount:
            mkEffectFn2 \node editor ->
              (editor # notNull # writeRef ref)
          }
        ]
