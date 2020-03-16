module Storybook.Decorator.FullScreen where

import Prelude
import Data.Array (find)
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse_)
import Effect (Effect)
import JSS (jss, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, component, element, useState, (/\))
import React.Basic.Hooks as React
import Simple.JSON (readJSON_, writeJSON)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)
import Yoga.Font.PragmataPro as PragmataPro
import Yoga.Font.Rubik as Rubik
import Yoga.Theme (fromTheme)
import Yoga.Theme.CSSBaseline (mkCssBaseline)
import Yoga.Theme.Default (darkTheme, lightTheme)
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
            , height: "100vh"
            }
        , children:
          [ element themeSwitcher
              { kids: [ child ]
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

type DefaultTheme
  = { name ∷ String, theme ∷ CSSTheme }

mkThemeSwitcher ∷
  Effect
    ( ReactComponent { defaultTheme ∷ DefaultTheme, themes ∷ Array DefaultTheme, kids ∷ Array JSX }
    )
mkThemeSwitcher = do
  themeProvider <- mkThemeProvider
  baseline <- mkCssBaseline
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
          , "@font-face": jss (Rubik.fontFamilies <> [ PragmataPro.fontFamily ])
          }
  storage <- window >>= localStorage
  saved ∷ (Maybe DefaultTheme) <- getItem "theme" storage <#> (_ >>= readJSON_)
  component "ThemeSwitcher" \{ defaultTheme, themes, kids } -> React.do
    classes <- useStyles {}
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
          , children:
            pure
              $ element baseline
                  { kids:
                    [ R.div
                        { style: css { backgroundColor: theme.backgroundColour, width: "100%", height: "100%" }
                        , children:
                          [ themeSelect ] <> kids
                        }
                    ]
                  }
          }
