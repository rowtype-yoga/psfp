module Yoga.Theme.Syntax where

import Prelude
import Color as Color
import React.Basic.DOM (css)
import React.Basic.SyntaxHighlighter.Component (HighlighterTheme)
import Yoga.Theme (increaseContrastTo)
import Yoga.Theme.Types (CSSTheme)

mkHighlighterTheme ∷ CSSTheme -> HighlighterTheme
mkHighlighterTheme theme =
  let
    contrasted = increaseContrastTo 6.0 theme.backgroundColour
  in
    { hljs:
      css
        { display: "inline"
        , overflowX: "auto"
        }
    , "hljs-keyword":
      css
        { color: Color.cssStringRGBA (contrasted (Color.rotateHue (-140.0) theme.highlightColour))
        , fontWeight: "normal"
        }
    , "hljs-symbol":
      css
        { color: Color.cssStringRGBA (contrasted (Color.rotateHue (-140.0) theme.highlightColour))
        }
    , "hljs-type":
      css
        { color: Color.cssStringRGBA (contrasted (Color.rotateHue (-80.0) theme.highlightColour))
        , fontWeight: "normal"
        }
    , "hljs-string":
      css
        { color: Color.cssStringRGBA (contrasted (Color.rotateHue (-20.0) theme.highlightColour))
        }
    , "hljs-title":
      css
        { color: Color.cssStringRGBA (contrasted (Color.rotateHue (-120.0) theme.highlightColour))
        , fontStyle: "normal"
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
        { color: "tomato"
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
    , "hljs-comment":
      css
        { color: "#777"
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
