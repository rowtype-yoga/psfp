module Theme.Provider
  ( mkThemeProvider
  ) where

import Effect (Effect)
import React.Basic (JSX, ReactComponent)
import Theme.Types (CSSTheme)

foreign import mkThemeProviderImpl ∷
  ∀ theme.
  Effect (ReactComponent { theme ∷ { | theme }, children ∷ Array JSX })

mkThemeProvider ∷
  Effect (ReactComponent { children ∷ Array JSX, theme ∷ CSSTheme })
mkThemeProvider = mkThemeProviderImpl
