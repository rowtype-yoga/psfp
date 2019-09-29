module Decorator.FullScreen where

import Prelude

import Data.Array (find)
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse_)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, component, element, useState, (/\))
import React.Basic.Hooks as React
import Simple.JSON (readJSON_, writeJSON)
import Theme.Default (darkTheme, lightTheme)
import Theme.Provider (mkThemeProvider)
import Theme.Types (CSSTheme, fromTheme)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

fullScreenDecorator ∷ JSX -> Effect JSX
fullScreenDecorator child = do
  let
    dark = fromTheme darkTheme
    light = fromTheme lightTheme
  themeSwitcher <- mkThemeSwitcher
  pure
    $ R.div
        { style:
          css
            { width: "100vw"
            , minHeight: "900px"
            , minWidth: "1440px"
            , height: "100vh"
            }
        , children:
          [ element themeSwitcher
              { children: [ child ]
              , defaultTheme: { name: "Dark", theme: dark }
              , themes:
                  [ { name: "Dark", theme: dark }
                  , { name: "Light", theme: light }
                  ]
              }
          ]
        }

type ThemesWithNames
  = Map String CSSTheme

type DefaultTheme = { name ∷ String, theme ∷ CSSTheme }

mkThemeSwitcher ∷
  Effect
    ( ReactComponent { defaultTheme ∷ DefaultTheme, themes ∷ Array DefaultTheme, children ∷ Array JSX }
    )
mkThemeSwitcher = do
  themeProvider <- mkThemeProvider
  storage <- window >>= localStorage
  saved ∷ (Maybe DefaultTheme) <- getItem "theme" storage <#> (_ >>= readJSON_)
  component "ThemeSwitcher" \{ defaultTheme, themes, children } -> React.do
    { theme, name } /\ modTheme <- useState $ fromMaybe defaultTheme saved
    let
      setTheme newTheme = do
        setItem "theme" (writeJSON newTheme) storage
        modTheme (const newTheme)
    let
      handleClicked maybeValue =
        traverse_ setTheme do
          value <- maybeValue
          themes # find \x -> x.name == value
    let
      themeSelect =
        R.select
          { onChange: handler targetValue handleClicked
          , value: name
          , children:
            themes <#> \x ->
                  R.option
                    { value: x.name
                    , key: x.name
                    , children: [ R.text x.name ]
                    }
          }
    pure
      $ element themeProvider
          { theme
          , children:
            [ R.div
                { style: css { backgroundColor: theme.backgroundColour, width: "100%", height: "100%" }
                , children:
                  [ themeSelect ] <> children
                }
            ]
          }
