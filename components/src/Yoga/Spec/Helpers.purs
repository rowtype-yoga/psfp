module Yoga.Spec.Helpers where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Prim.Row (class Lacks)
import React.Basic.Hooks (ReactComponent, component, element)
import Yoga.Theme (fromTheme)
import Yoga.Theme.Default (darkTheme, lightTheme)
import Yoga.Theme.Provider (mkThemeProvider)
import Yoga.Theme.Types (CSSTheme)

withTheme ∷
  ∀ props.
  Lacks "children" props =>
  Lacks "ref" props =>
  Lacks "key" props =>
  CSSTheme ->
  Effect (ReactComponent { | props }) ->
  Aff (ReactComponent { | props })
withTheme theme mkComp =
  liftEffect do
    themeProvider <- mkThemeProvider
    comp <- mkComp
    component "ThemeWrapper" \(props ∷ { | props }) -> React.do
      pure
        $ element themeProvider
            { theme
            , children: [ element comp props ]
            }

withDarkTheme ∷
  ∀ props.
  Lacks "children" props =>
  Lacks "ref" props =>
  Lacks "key" props =>
  Effect (ReactComponent (Record props)) ->
  Aff (ReactComponent (Record props))
withDarkTheme = withTheme (fromTheme darkTheme)

withLightTheme ∷
  ∀ props.
  Lacks "children" props =>
  Lacks "ref" props =>
  Lacks "key" props =>
  Effect (ReactComponent (Record props)) ->
  Aff (ReactComponent (Record props))
withLightTheme = withTheme (fromTheme lightTheme)
