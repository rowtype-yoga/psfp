module Yoga.Spec.Helpers where

import Prelude
import CSS (rem)
import Color as Color
import Effect (Effect)
import JSS (jss)
import Prim.Row (class Lacks)
import React.Basic.Hooks (ReactComponent, reactComponent, element)
import Yoga.Font (FontFamily)
import Yoga.Theme (fromTheme)
import Yoga.Theme.CSSBaseline (mkCssBaseline)
import Yoga.Theme.Provider (mkThemeProvider)
import Yoga.Theme.Types (CSSTheme, Theme)

withTheme ∷
  ∀ props.
  Lacks "children" props =>
  Lacks "ref" props =>
  Lacks "key" props =>
  CSSTheme ->
  Effect (ReactComponent { | props }) ->
  Effect (ReactComponent { | props })
withTheme theme mkComp = do
  themeProvider <- mkThemeProvider
  baseline <- mkCssBaseline (jss ([] ∷ _ FontFamily))
  comp <- mkComp
  reactComponent "ThemeWrapper" \(props ∷ { | props }) -> React.do
    pure
      $ element themeProvider
          { theme
          , children: [ element baseline { kids: [ element comp props ] } ]
          }

-- Remove fonts because they require webpack to load
specTheme ∷ Theme
specTheme =
  { backgroundColour: Color.hsl 238.0 0.18 0.20
  , textColour: Color.hsl 225.0 0.28 0.90
  , interfaceColour: Color.hsl 225.0 0.48 0.12
  , highlightColour: Color.hsl 285.0 0.88 0.72
  , altHighlightColour: Color.hsl 84.0 0.617 0.631
  , textFontFamily: pure "sans-serif"
  , headingFontFamily: pure "sans-serif"
  , codeFontFamily: pure "monospace"
  , yellow: Color.hsl 36.0 0.82 0.76
  , green: Color.hsl 84.0 0.617 0.631
  , purple: Color.hsl 276.0 0.677 0.745
  , red: Color.rgb 255 88 116
  , blue: Color.rgb 130 170 255
  , grey: Color.rgb 150 150 150
  , white: Color.rgb 250 250 250
  , measure: "60ch"
  , borderThin: "0.125rem"
  , ratio: 1.5
  , s0: 1.0 # rem
  }

withSpecTheme ∷
  ∀ props.
  Lacks "children" props =>
  Lacks "ref" props =>
  Lacks "key" props =>
  Effect (ReactComponent (Record props)) ->
  Effect (ReactComponent (Record props))
withSpecTheme = withTheme (fromTheme specTheme)
