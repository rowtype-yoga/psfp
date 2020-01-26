module Container.Component where

import Prelude
import CSS.Safer (cssSafer)
import CompileEditor.Component (mkCompileEditor)
import Container.Header (mkHeader)
import Container.Landing (mkLandingPage)
import Container.Sidebar (mkSidebar, mkSidebarLink)
import Data.Nullable as Nullable
import Effect (Effect)
import Polyfill.SmoothScrolling (smoothScrollPolyfill)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element, useRef, useState, (/\))
import React.Basic.Hooks as React
import SVG.Icon (appendIcon, applyflippedIcon, bindIcon, mapIcon)
import Theme.Provider (mkThemeProvider)
import Theme.Styles (makeStyles)
import Theme.Types (CSSTheme)

mkContainer ∷ Effect (ReactComponent { theme ∷ CSSTheme, children ∷ Array JSX })
mkContainer = do
  themeProvider <- mkThemeProvider
  containerContent <- mkContainerContent
  component "Container" \{ theme, children } -> React.do
    pure
      $ element themeProvider
          { theme
          , children:
            [ element containerContent { children }
            ]
          }

mkContainerContent ∷ Effect (ReactComponent { children ∷ Array JSX })
mkContainerContent = do
  smoothScrollPolyfill
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      --  { "@global": cssSafer { "*": { outline: "1px solid red" } }
      { container:
        cssSafer
          { backgroundColor: theme.backgroundColour
          , fontFamily: theme.textFontFamily
          , color: theme.textColour
          , display: "grid"
          , transition: "0.2s ease-in-out"
          , gridTemplateAreas:
            "'landing landing landing'"
              <> "'header header header' "
              <> "'nav content content'"
          -- , "footer footer footer"
          -- , gridTemplateColumns: "max-content auto"
          , minWidth: "100%"
          , maxWidth: "100%"
          }
      , content:
        cssSafer
          { gridArea: "content"
          , minHeight: "200vh"
          }
      , icon: cssSafer { fill: "theme.textColour" }
      }
  landingPage <- mkLandingPage
  sidebar <- mkSidebar
  header <- mkHeader
  sidebarLink <- mkSidebarLink
  editor <- mkCompileEditor
  component "ContainerContent" \{ children } -> React.do
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
                , children:
                  [ element sidebarLink { name: "Learn", icon: element appendIcon {}, collapsed }
                  , element sidebarLink { name: "Try", icon: element bindIcon {}, collapsed }
                  , element sidebarLink { name: "Share", icon: element applyflippedIcon {}, collapsed }
                  , element sidebarLink { name: "Jobs", icon: element mapIcon {}, collapsed }
                  ]
                }
            , element header {}
            , R.div { className: classes.content, children: [ element editor { initialCode } ] <> children }
            ]
          }

initialCode :: String
initialCode =
  """module Main where

import Batteries

main :: Effect Unit
main ="""
