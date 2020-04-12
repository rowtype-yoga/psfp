module React.Basic.SyntaxHighlighter.Component where

import React.Basic (ReactComponent)
import React.Basic.DOM (CSS)

type HighlighterTheme
  = { hljs ∷ CSS
    , "hljs-addition" ∷ CSS
    , "hljs-attribute" ∷ CSS
    , "hljs-built_in" ∷ CSS
    , "hljs-bullet" ∷ CSS
    , "hljs-comment" ∷ CSS
    , "hljs-deletion" ∷ CSS
    , "hljs-doctag" ∷ CSS
    , "hljs-emphasis" ∷ CSS
    , "hljs-keyword" ∷ CSS
    , "hljs-link" ∷ CSS
    , "hljs-literal" ∷ CSS
    , "hljs-meta" ∷ CSS
    , "hljs-name" ∷ CSS
    , "hljs-quote" ∷ CSS
    , "hljs-section" ∷ CSS
    , "hljs-selector-tag" ∷ CSS
    , "hljs-string" ∷ CSS
    , "hljs-strong" ∷ CSS
    , "hljs-subst" ∷ CSS
    , "hljs-symbol" ∷ CSS
    , "hljs-template-tag" ∷ CSS
    , "hljs-template-variable" ∷ CSS
    , "hljs-title" ∷ CSS
    , "hljs-type" ∷ CSS
    , "hljs-variable" ∷ CSS
    }

foreign import syntaxHighlighterImpl ∷
  ReactComponent
    { language ∷ String
    , style ∷ HighlighterTheme
    , children ∷ String
    }
