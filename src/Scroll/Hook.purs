module Scroll.Hook where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import React.Basic.Hooks (Hook, UseLayoutEffect, UseState, coerceHook, useLayoutEffect, useState, (/\))
import React.Basic.Hooks as React
import Scroll.Listener as Scroll
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventListener, eventListener)
import Web.HTML (window)

newtype UseScrollYPosition hooks
  = UseScrollYPosition (UseLayoutEffect Unit (UseState Number hooks))

derive instance ntUseScrollYPosition ∷ Newtype (UseScrollYPosition hooks) _

useScrollYPosition ∷ Hook UseScrollYPosition Number
useScrollYPosition =
  coerceHook React.do
    scrollY /\ modifyScrollY <- useState 0.0
    let setScrollY = modifyScrollY <<< const
    useLayoutEffect unit do
          listener <- makeListener setScrollY
          Scroll.registerListener listener
    pure scrollY

makeListener ∷ (Number -> Effect Unit) -> Effect EventListener
makeListener setPosition = do
  eventListener $ const do
    win <- window
    let yPos = (unsafeCoerce win).scrollY
    setPosition yPos
