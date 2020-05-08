module Container.Sidebar where

import Prelude
import Data.Foldable (intercalate)
import Data.Monoid (guard)
import Effect (Effect)
import JSS (jss, jssClasses)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import Yoga.SVG.Icon (ActiveArrowDirection(..), mkMenu)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

mkSidebar ∷
  Effect
    ( ReactComponent
        { kids ∷ Array JSX
        , modifyCollapsed ∷ (Boolean -> Boolean) -> Effect Unit
        , collapsed ∷ Boolean
        }
    )
mkSidebar = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { sidebar:
            jss
              { background: theme.interfaceColourLighter
              , fontFamily: intercalate ", " theme.textFontFamily
              , borderRadius: "0 12px 12px 0"
              , color: theme.textColour
              , position: "absolute"
              , marginTop: "calc(100vh + 20px)"
              , paddingBottom: "20px"
              , display: "flex"
              , flexDirection: "column"
              , transition: "0.4s"
              , transitionTimingFunction: "cubic-bezier(.56,-0.32,.36,1)"
              , overflowX: "hidden"
              , zIndex: 24
              , boxShadow:
                "0px 0px 60px rgba(0,0,0,0."
                  <> (if theme.isLight then "17" else "43")
                  <> ")"
              }
          , sidebarCollapsed: jss { width: "70px" }
          , sidebarExpanded: jss { width: "220px" }
          , logo:
            jss
              { width: "60px"
              , height: "60px"
              , padding: "0 5px 0 5px"
              , transition: "0.2s ease-in-out"
              , transitionDelay: "0.1s"
              , alignSelf: "flex-end"
              }
          , logoCollapsed:
            jss
              {} -- transform: "rotate(180deg)"}
          }
  menu <- mkMenu
  component "Sidebar" \{ kids, collapsed, modifyCollapsed } -> React.do
    classes <- useStyles {}
    pure
      $ R.nav
          { className: classes.sidebar <> " " <> if collapsed then classes.sidebarCollapsed else classes.sidebarExpanded
          , children:
            [ R.ul
                { className: classes.logo <> " " <> guard collapsed classes.logoCollapsed
                , onClick: handler_ $ (modifyCollapsed not)
                , children:
                  [ element menu
                      { activeArrowDirection: if collapsed then ArrowPointsRight else ArrowPointsLeft }
                  ]
                }
            ]
              <> kids
          }

mkSidebarLink ∷ Effect (ReactComponent { name ∷ String, icon ∷ JSX, collapsed ∷ Boolean })
mkSidebarLink = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { sidebarEntry:
            jss
              { fontFamily: intercalate ", " theme.headingFontFamily
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
              -- , "&:hover": jss{ opacity: "1.0"}
              }
          , label:
            jss
              { fontFamily: intercalate ", " theme.textFontFamily
              , color: theme.textColour
              , paddingLeft: "20px"
              , textTransform: "uppercase"
              , lineHeight: "60px"
              , fontSize: "1.4em"
              }
          , icon:
            jss
              { width: "50px"
              , height: "50px"
              , padding: "5px 10px 5px 5px"
              , transition: "0.2s ease-in-out"
              , transitionDelay: "0.1s"
              , alignSelf: "flex-end"
              , fill: theme.textColour
              }
          }
  component "SidebarLink" \{ name, icon, collapsed } -> React.do
    classes <- useStyles {}
    pure
      $ R.li
          { className: classes.sidebarEntry
          , children:
            [ R.div
                { className: classes.label
                , children: [ R.text name ]
                }
            , R.div
                { className: classes.icon
                , children: [ icon ]
                }
            ]
          }
