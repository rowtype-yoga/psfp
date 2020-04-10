module Yoga.Highlighter.Monaco where

import Prelude
import Control.Promise as Promise
import Effect.Aff (Aff)
import Yoga.Editor (Editor, colorizeImpl)
import Yoga.Highlighter.Types (HTMLString(..), Highlighter, Language(..))

monacoHighlighter ∷ Editor -> { | Highlighter () }
monacoHighlighter editor =
  { highlight
  }
  where
  langToString ∷ Language -> String
  langToString Purescript = "purescript"
  highlight ∷ String -> Language -> Aff HTMLString
  highlight code lang =
    HTMLString
      <$> Promise.toAff
          ( colorizeImpl code
              (langToString lang)
              { tabSize: 2 }
              editor
          )
