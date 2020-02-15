module Container.Component where

import Prelude
import CSS.Safer (cssSafer)
import CompileEditor.Component (mkCompileEditor)
import Container.Header (mkHeader)
import Container.Landing (mkLandingPage)
import Container.Sidebar (mkSidebar, mkSidebarLink)
import Data.Nullable as Nullable
import Effect (Effect)
import Milkis.Impl.Window (windowFetch)
import Polyfill.SmoothScrolling (smoothScrollPolyfill)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element, useRef, useState, (/\))
import React.Basic.Hooks as React
import SVG.Icon (appendIcon, applyflippedIcon, bindIcon, mapIcon)
import Theme.Provider (mkThemeProvider)
import Theme.Styles (makeStyles)
import Theme.Types (CSSTheme)

mkContainer ∷ Effect (ReactComponent { theme ∷ CSSTheme, kids ∷ Array JSX })
mkContainer = do
  themeProvider <- mkThemeProvider
  containerContent <- mkContainerContent
  component "Container" \{ theme, kids } -> React.do
    pure
      $ element themeProvider
          { theme
          , children:
            [ element containerContent { kids }
            ]
          }

mkContainerContent ∷ Effect (ReactComponent { kids ∷ Array JSX })
mkContainerContent = do
  smoothScrollPolyfill
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      --  { "@global": cssSafer { "*": { outline: "1px solid red" } }
      { container:
        --"@global": -- { "*": { outline: "1px solid red" } } 
        cssSafer
          { fontFamily: theme.textFontFamily
          , color: theme.textColour
          , display: "flex"
          , flexDirection: "column"
          , width: "100%"
          , minHeight: "200%"
          }
      , content:
        cssSafer
          { backgroundColor: theme.backgroundColour
          , padding: "20px"
          , minHeight: "calc(100vh - 40px)"
          , paddingLeft: "100px"
          , zIndex: 0
          }
      , icon: cssSafer { fill: "theme.textColour" }
      }
  landingPage <- mkLandingPage
  sidebar <- mkSidebar
  header <- mkHeader
  sidebarLink <- mkSidebarLink
  editor <- mkCompileEditor windowFetch
  component "ContainerContent" \{ kids } -> React.do
    classes <- useStyles
    collapsed /\ modifyCollapsed <- useState true
    editorRef <- useRef Nullable.null
    pure
      $ R.div
          { className: classes.container
          , children:
            [ element landingPage {}
            , element sidebar
                { collapsed
                , modifyCollapsed
                , kids:
                  [ element sidebarLink { name: "Learn", icon: element appendIcon {}, collapsed }
                  , element sidebarLink { name: "Try", icon: element bindIcon {}, collapsed }
                  , element sidebarLink { name: "Share", icon: element applyflippedIcon {}, collapsed }
                  , element sidebarLink { name: "Jobs", icon: element mapIcon {}, collapsed }
                  ]
                }
            , R.div
                { className: classes.content
                , children: [ element editor { initialCode, height: "50vh" } ] <> kids
                }
            ]
          }

initialCode :: String
initialCode =
  """module Main where

import Batteries

main :: Effect Unit
main = log "Let's do this!""""
