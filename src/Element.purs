module Element where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, element, useEffect, useState, (/\))
import React.Basic.Hooks as React
import Theme.Provider (mkThemeProvider)
import Theme.Styles (makeStyles)
import Theme.Types (Theme)

mkToggleButton ∷ Effect (ReactComponent { label ∷ String })
mkToggleButton = do
  useStyles <-
    makeStyles \(theme ∷ Theme) -> -- do
      { button:
        css
          { backgroundColor: theme.highlightColour
          , color: theme.textColour
          , fontFamily: theme.textFontFamily
          , fontSize: "1em"
          , paddingTop: "10px"
          , paddingBottom: "10px"
          , paddingRight: "20px"
          , paddingLeft: "20px"
          , border: "solid 0px black"
          , borderRadius: "5px"
          }
      }
  component "ToggleButton" \{ label } -> React.do
    classes <- useStyles
    on /\ setOn <- useState false
    useEffect on do
      log $ "State: " <> if on then "On" else "Off"
      pure (pure unit)
    pure
      $ R.button
          { onClick: handler_ $ setOn not
          , className: classes.button
          , children:
            [ R.text label
            , R.text if on then " On" else " Off"
            ]
          }

mkToggleButtonContainer ∷ Effect (ReactComponent {})
mkToggleButtonContainer = do
  themeProvider <- mkThemeProvider
  toggleButton <- mkToggleButton
  component "Container" \_ ->
    pure
      $ R.div
          { children:
            [ element toggleButton { label: "A" }
            , element toggleButton { label: "B" }
            ]
          }
