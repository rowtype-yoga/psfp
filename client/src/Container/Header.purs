module Container.Header where

import Prelude
import CSS.Safer (cssSafer)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null)
import Effect (Effect)
import Effect.Class.Console (log)
import React.Basic.DOM as R
import React.Basic.Hooks (type (/\), Hook, ReactComponent, Ref, UseLayoutEffect, UseRef, UseState, coerceHook, component, element, readRefMaybe, useLayoutEffect, useRef, useState, (/\))
import React.Basic.Hooks as React
import SVG.Icon (trianglelogoIcon)
import Scroll.Listener as Scroll
import Theme.Styles (makeStyles)
import Theme.Types (CSSTheme)
import Typography.Header (HeadingLevel(..), mkH)
import Web.DOM (Node)
import Web.Event.EventTarget (EventListener, eventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement (offsetTop)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (scrollY)

mkHeader ∷ Effect (ReactComponent {})
mkHeader = do
  h <- mkH
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      { header:
        cssSafer
          { backgroundColor: theme.interfaceColour
          , borderBottom: "0"
          , fontFamily: theme.headingFontFamily
          , gridArea: "header"
          , display: "flex"
          , alignItems: "center"
          , justifyContent: "flex-end"
          , width: "100%"
          , height: "80px"
          , paddingTop: "10px"
          , paddingBottom: "10px"
          }
      , sticky: cssSafer { position: "fixed", top: "0", zIndex: 10 }
      , title: cssSafer { paddingRight: "30px" }
      , logo:
        cssSafer
          { width: "70px"
          , height: "70px"
          , padding: "5px"
          , marginTop: "9px"
          , marginRight: "7px"
          , fill: theme.textColour
          }
      }
  component "Header" \{} -> React.do
    classes <- useStyles
    shouldBeSticky /\ nodeRef <- useShouldBeSticky
    pure
      $ R.header
          { ref: nodeRef
          , className: classes.header <> " " <> guard shouldBeSticky classes.sticky
          , children:
            [ element h { level: H2, text: "Rowtype Yoga", className: Just classes.title }
            , R.div { children: [ element trianglelogoIcon {} ], className: classes.logo }
            ]
          }

newtype UseShouldBeSticky hooks
  = UseShouldBeSticky (UseLayoutEffect Unit (UseState Boolean (UseRef (Nullable Node) hooks)))

derive instance ntUseShouldBeSticky ∷ Newtype (UseShouldBeSticky hooks) _

useShouldBeSticky ∷ Hook UseShouldBeSticky (Boolean /\ Ref (Nullable Node))
useShouldBeSticky =
  coerceHook React.do
    nodeRef <- useRef null
    mustBeSticky /\ modifyMustBeSticky <- useState false
    let
      setSticky = modifyMustBeSticky <<< const
    useLayoutEffect unit do
      maybeNode <- readRefMaybe nodeRef
      case maybeNode >>= HTMLElement.fromNode of
        Nothing -> do
          log "Could not register listener because there was no node"
          pure (pure unit)
        Just element -> do
          listener <- makeListener setSticky element
          Scroll.registerListener listener
    pure (mustBeSticky /\ nodeRef)

makeListener ∷ (Boolean -> Effect Unit) -> HTMLElement -> Effect EventListener
makeListener setSticky element = do
  nodeStartPos <- offsetTop element
  eventListener
    $ const do
        yPos <- window >>= scrollY <#> toNumber
        setSticky $ nodeStartPos < yPos
