module Container.Component where

import Prelude
import Container.Header (mkHeader)
import Container.Landing (mkLandingPage)
import Container.Sidebar (mkSidebar, mkSidebarLink)
import Data.Nullable as Nullable
import Effect (Effect)
import JSS (jss, jssClasses)
import Milkis.Impl.Window (windowFetch)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element, useRef, useState, (/\))
import React.Basic.Hooks as React
import Yoga.CompileEditor.Component (mkCompileEditor)
import Yoga.Compiler.Api (apiCompiler)
import Yoga.Polyfill.SmoothScrolling (smoothScrollPolyfill)
import Yoga.SVG.Icon (appendIcon, applyflippedIcon, bindIcon, mapIcon)
import Yoga.Theme.Provider (mkThemeProvider)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

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
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { container:
            jss
              { color: theme.textColour
              , display: "flex"
              , flexDirection: "column"
              , width: "100%"
              , minHeight: "200%"
              }
          , content:
            jss
              { backgroundColor: theme.backgroundColour
              , padding: "20px"
              , minHeight: "calc(100vh - 40px)"
              , paddingLeft: "100px"
              , zIndex: 0
              }
          , icon: jss { fill: theme.textColour }
          }
  landingPage <- mkLandingPage
  sidebar <- mkSidebar
  header <- mkHeader
  sidebarLink <- mkSidebarLink
  editor <- mkCompileEditor (apiCompiler windowFetch)
  component "ContainerContent" \{ kids } -> React.do
    classes <- useStyles {}
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
                , children: [ element editor { initialCode, height: "50vh", language: "purescript" } ] <> kids
                }
            ]
          }

initialCode ∷ String
initialCode =
  """module Main where

import Batteries

main :: Effect Unit
main = log "Let's do this!""""
