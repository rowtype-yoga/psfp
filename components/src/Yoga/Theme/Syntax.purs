module Yoga.Theme.Syntax where

import Prelude
import Color (ColorSpace(..))
import Color as Color
import Color as Color
import React.Basic.DOM (css)
import React.Basic.SyntaxHighlighter.Component (HighlighterTheme)
import Yoga.Theme.Types (CSSTheme)

mkHighlighterTheme ∷ CSSTheme -> HighlighterTheme
mkHighlighterTheme theme =
  { hljs:
    css
      { display: "inline"
      , overflowX: "auto"
      , color: Color.cssStringRGBA $ Color.mix HSL (Color.desaturate 0.5 theme.highlightColour) (Color.desaturate 0.3 theme.textColour) 0.4
      }
  , "hljs-keyword":
    css
      { color: Color.cssStringRGBA theme.highlightColour
      , fontWeight: "normal"
      }
  , "hljs-symbol":
    css
      { color: Color.cssStringRGBA $ theme.highlightColour
      }
  , "hljs-type":
    css
      { color: Color.cssStringRGBA theme.highlightColour
      , fontWeight: "normal"
      }
  , "hljs-string":
    css
      { color: Color.cssStringRGBA $ theme.highlightColour # Color.rotateHue (-15.0)
      }
  , "hljs-title":
    css
      { color: Color.cssStringRGBA $ theme.highlightColour # Color.rotateHue 15.0
      }
  , "hljs-comment":
    css
      { color: Color.cssStringRGBA $ theme.grey
      }
  , "hljs-selector-tag":
    css
      { color: "yellow"
      , fontWeight: "bold"
      }
  , "hljs-literal":
    css
      { color: "green"
      , fontWeight: "bold"
      }
  , "hljs-section":
    css
      { color: "darkslateblue"
      , fontWeight: "bold"
      }
  , "hljs-link":
    css
      { color: "yellow"
      }
  , "hljs-subst":
    css
      { color: "#ddd"
      }
  , "hljs-name":
    css
      { color: "#d88"
      , fontWeight: "bold"
      }
  , "hljs-attribute":
    css
      { color: "hotpink"
      }
  , "hljs-bullet":
    css
      { color: "#d88"
      }
  , "hljs-built_in":
    css
      { color: "#d88"
      }
  , "hljs-addition":
    css
      { color: "#d88"
      }
  , "hljs-variable":
    css
      { color: "#d88"
      }
  , "hljs-template-tag":
    css
      { color: "#d88"
      }
  , "hljs-template-variable":
    css
      { color: "#d88"
      }
  , "hljs-quote":
    css
      { color: "#777"
      }
  , "hljs-deletion":
    css
      { color: "#777"
      }
  , "hljs-meta":
    css
      { color: "#777"
      }
  , "hljs-doctag":
    css
      { fontWeight: "bold"
      }
  , "hljs-strong":
    css
      { fontWeight: "bold"
      }
  , "hljs-emphasis":
    css
      { fontStyle: "italic"
      }
  }
