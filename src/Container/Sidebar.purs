module Container.Sidebar where

import Prelude

import CSS.Safer (cssSafer)
import Data.Monoid (guard)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import SVG.Icon (ActiveArrowDirection(..), mkMenu)
import Theme.Styles (classNames, makeStyles)
import Theme.Types (CSSTheme)

mkSidebar ∷
  Effect
    ( ReactComponent
        { children ∷ Array JSX
        , modifyCollapsed ∷ (Boolean -> Boolean) -> Effect Unit
        , collapsed ∷ Boolean
        }
    )
mkSidebar = do
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      { sidebar:
        cssSafer
          { background: theme.backgroundColour
          , fontFamily: theme.textFontFamily
          , color: theme.textColour
          , gridArea: "nav"
          , display: "flex"
          , flexDirection: "column"
          , minHeight: "calc(100vh, -80px)"
          , transition: "0.2s ease-in-out"
          , overflowX: "hidden"
          }
      , sidebarCollapsed: cssSafer { width: "70px" }
      , sidebarExpanded: cssSafer { width: "250px" }
      , logo:
        cssSafer
          { width: "60px"
          , height: "60px"
          , padding: "0 5px 0 5px"
          , transition: "0.2s ease-in-out"
          , transitionDelay: "0.1s"
          , alignSelf: "flex-end"
          }
      , logoCollapsed:
        cssSafer
          {} -- transform: "rotate(180deg)"}
      }
  menu <- mkMenu
  component "Sidebar" \{ children, collapsed, modifyCollapsed } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.nav
          { className:
            classes
              [ _.sidebar
              , if collapsed then _.sidebarCollapsed else _.sidebarExpanded
              ]
          , children:
            [ R.ul
                { className: classes [ _.logo, guard collapsed _.logoCollapsed ]
                , onClick: handler_ $ (modifyCollapsed not)
                , children:
                  [ element menu
                      { activeArrowDirection: if collapsed then ArrowPointsRight else ArrowPointsLeft }
                  ]
                }
            ]
              <> children
          }

mkSidebarLink ∷ Effect (ReactComponent { name ∷ String, icon ∷ JSX, collapsed ∷ Boolean })
mkSidebarLink = do
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      { sidebarEntry:
        cssSafer
          { fontFamily: theme.headingFontFamily
          , alignSelf: "flex-end"
          , justifyContent: "space-between"
          , alignContent: "stretch"
          , color: theme.textColour
          , gridArea: "nav"
          , display: "flex"
          , flexDirection: "row"
          , overflowX: "hidden"
          , width: "190px"
          , paddingLeft: "5px"
          , borderRadius: "5px 0 0 5px"
          , cursor: "pointer"
          , "&:hover":
              { background: theme.highlightColour
              }
          -- , "&:hover": cssSafer{ opacity: "1.0"}
          }
      , label:
        cssSafer
          { fontFamily: theme.textFontFamily
          , color: theme.textColour
          , paddingLeft: "20px"
          , textTransform: "uppercase"
          , lineHeight: "60px"
          , fontSize: "1.4em"
          }
      , icon:
        cssSafer
          { width: "50px"
          , height: "50px"
          , padding: "5px 10px 5px 5px"
          , transition: "0.2s ease-in-out"
          , transitionDelay: "0.1s"
          , alignSelf: "flex-end"
          , fill: theme.textColour <> " !important"
          }
      }
  component "SidebarLink" \{ name, icon, collapsed } -> React.do
    rawClasses <- useStyles
    let
      classes = flip classNames rawClasses
    pure
      $ R.li
          { className: classes [ _.sidebarEntry ]
          , children:
            [ R.div
                { className: classes [ _.label ]
                , children: [ R.text name ]
                }
            , R.div
                { className: classes [ _.icon ]
                , children: [ icon ]
                }
            ]
          }
