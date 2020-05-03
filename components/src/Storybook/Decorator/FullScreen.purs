module Storybook.Decorator.FullScreen where

import Prelude
import Data.Array (find)
import Data.Traversable (traverse_)
import Effect (Effect)
import JSS (jss, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, component, element, useEffect, useState, (/\))
import React.Basic.Hooks as React
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)
import Yoga.Theme (fromTheme)
import Yoga.Theme.CSSBaseline (mkCssBaseline)
import Yoga.Theme.Default (darkTheme, lightTheme)
import Yoga.Theme.Default as Default
import Yoga.Theme.Provider (mkThemeProvider)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

fullScreenDecorator ∷ Effect JSX -> Effect JSX
fullScreenDecorator mkChild = do
  let
    dark = fromTheme darkTheme

    light = fromTheme lightTheme
  child <- mkChild
  themeSwitcher <- mkThemeSwitcher
  pure
    $ R.div
        { style:
          css
            { minWidth: "100vw"
            , minHeight: "100vh"
            , display: "flex"
            }
        , children:
          [ element themeSwitcher
              { child
              , defaultTheme: { name: "Dark", theme: dark }
              , themes:
                [ { name: "Dark", theme: dark }
                , { name: "Light", theme: light }
                ]
              }
          ]
        }

type NamedTheme
  = { name ∷ String, theme ∷ CSSTheme }

mkThemeSwitcher ∷
  Effect
    ( ReactComponent
        { defaultTheme ∷ NamedTheme
        , themes ∷ Array NamedTheme
        , child ∷ JSX
        }
    )
mkThemeSwitcher = do
  themeProvider <- mkThemeProvider
  baseline <- mkCssBaseline Default.fontFaces
  useStyles <-
    makeStylesJSS
      $ jssClasses \_ ->
          { selector:
            jss
              { position: "absolute"
              , zIndex: 20
              , top: 10
              , right: 10
              }
          }
  storage <- window >>= localStorage
  component "ThemeSwitcher" \{ defaultTheme, themes, child } -> React.do
    { theme, name } /\ modTheme <- useState defaultTheme
    useEffect unit do
      maybeSaved <- getItem "theme" storage
      traverse_ (modTheme <<< const) do
        saved <- maybeSaved
        find (\x -> x.name == saved) themes
      pure mempty
    useEffect name do
      setItem "theme" name storage
      pure mempty
    classes <- useStyles {}
    let
      handleClicked maybeValue =
        traverse_ (modTheme <<< const) do
          value <- maybeValue
          find (\x -> x.name == value) themes

      themeSelect =
        R.select
          { onChange: handler targetValue handleClicked
          , value: name
          , className: classes.selector
          , children:
            themes
              <#> \x ->
                  R.option
                    { value: x.name
                    , key: x.name
                    , children: [ R.text x.name ]
                    }
          }
    pure
      $ element themeProvider
          { theme
          , children: [ element baseline { kids: [ R.div_ [ themeSelect, child ] ] } ]
          }
