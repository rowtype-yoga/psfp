module YogaCard where

import Card.Component (mkCard)
import CompileEditor.Component (mkCompileEditor)
import Effect (Effect)
import Milkis.Impl (FetchImpl)
import React.Basic (JSX, ReactComponent)
import Theme (fromTheme)
import Theme.Default (darkTheme)
import Theme.Provider (mkThemeProvider)
import Theme.Types (CSSTheme)

makeCard ∷
  Effect
    ( ReactComponent
        { className ∷ String
        , kids ∷ Array JSX
        }
    )
makeCard = mkCard

mkEditor ∷
  FetchImpl ->
  Effect
    ( ReactComponent
        { height ∷ String
        , initialCode ∷ String
        }
    )
mkEditor = mkCompileEditor

makeThemeProvider :: Effect
  (ReactComponent
     { children :: Array JSX
     , theme :: CSSTheme
     }
  )
makeThemeProvider = mkThemeProvider

dark :: CSSTheme
dark = fromTheme darkTheme