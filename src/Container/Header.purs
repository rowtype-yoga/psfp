module Container.Header where

import Prelude

import Control.Alt ((<|>))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Nullable (null)
import Debug.Trace (spy)
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element, readRef, readRefMaybe, useEffect, useLayoutEffect, useRef, useState, writeRef, (/\))
import React.Basic.Hooks as React
import SVG.Icon (pslogoIcon)
import Theme.Styles (makeStyles)
import Theme.Types (CSSTheme)
import Typography.Header (HeadingLevel(..), mkH)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLElement (offsetTop)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (scrollY, toEventTarget)

mkHeader ∷ Effect (ReactComponent {})
mkHeader = do
  h <- mkH
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      { header:
        css
          { backgroundColor: theme.backgroundColour
          , borderBottom: "1px solid " <> theme.highlightColour
          , paddingLeft: "20px"
          , fontFamily: theme.textFontFamily
          , gridArea: "header"
          , display: "flex"
          , alignItems: "center"
          , width: "100vw"
          , height: "80px"
          }
      , sticky: css { position: "fixed", top: "0" }
      , logo:
        css
          { width: "70px"
          , height: "70px"
          , padding: "5px"
          , marginRight: "7px"
          , fill: theme.textColour
          }
      }
  useScrollYPosition <- mkUseScrollYPosition
  component "Header" \{} -> React.do
    classes <- useStyles
    mustBeSticky /\ modifyMustBeSticky <- useState false
    nodeY /\ modifyNodeY <- useState 0
    nodeOffset /\ modifyONP <- useState Nothing
    nodeRef <- useRef null
    scrollYPos <- useScrollYPosition
    useEffect scrollYPos do
      maybeNode <- readRefMaybe nodeRef
      let maybeElem = maybeNode >>= HTMLElement.fromNode
      case maybeElem of
        Nothing -> pure (pure unit)
        Just element -> do
          offset <- offsetTop element
          modifyONP case _ of
            Just x -> Just x
            Nothing -> Just offset
          let offset' = fromMaybe offset nodeOffset
          modifyMustBeSticky
            if toNumber scrollYPos > offset'
            then const true
            else const false
          pure (pure unit)

    pure
      $ R.header
          { ref: nodeRef
          , className: classes.header <> " " <> guard mustBeSticky classes.sticky
          , children:
            [ R.div { children: [ element pslogoIcon {} ], className: classes.logo }
            , element h { level: H2, text: "Purescript", className: Nothing }
            ]
          }

mkUseScrollYPosition = do
  initialPosition <- window >>= scrollY
  pure $ React.do
    scrollPosY /\ modifyScrollPosY <- useState initialPosition
    useLayoutEffect unit do
        target <- window <#> toEventTarget
        let eventType = EventType "scroll"
        listener <- eventListener
          \_ -> window >>= scrollY >>= \newPos -> modifyScrollPosY (const newPos)
        addEventListener eventType listener false target
        let _ = spy "registered event listener" unit
        pure $
          removeEventListener eventType listener false target
    pure $ scrollPosY
