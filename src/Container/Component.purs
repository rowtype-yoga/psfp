module Container.Component where

import Prelude

import Container.Header (mkHeader)
import Container.Sidebar (mkSidebar, mkSidebarLink)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks ((/\), ReactComponent, component, element, useState)
import React.Basic.Hooks as React
import SVG.Icon (appendIcon, applyflippedIcon, bindIcon, mapIcon, mapflippedIcon)
import Theme.Provider (mkThemeProvider)
import Theme.Styles (makeStyles)
import Theme.Types (Theme)

mkContainer ∷ Effect (ReactComponent { theme ∷ Theme, children ∷ Array JSX })
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
  sidebar <- mkSidebar
  header <- mkHeader
  useStyles <-
    makeStyles \(theme ∷ Theme) ->
      { container:
        css
          { backgroundColor: theme.backgroundColour
          , fontFamily: theme.textFontFamily
          , color: theme.foregroundColour
          , display: "grid"
          , gridTemplateAreas:
            "'nav header header' "
              <> "'nav content content'"
          -- , "footer footer footer"
          -- , gridTemplateColumns: "max-content auto"
          , minWidth: "100%"
          , minHeight: "100%"
          }
      , content: css { gridArea: "content" }
      }
  sidebarLink <- mkSidebarLink
  component "ContainerContent" \{ children } -> React.do
    classes <- useStyles
    collapsed /\ modifyCollapsed <- useState true
    pure
      $ R.div
          { className: classes.container
          , children:
            [ element sidebar
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
            , R.div { className: classes.content, children }
            ]
          }
