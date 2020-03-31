module React.Basic.Extra.Hooks.UseKeyUp where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Debug.Trace (spy)
import Effect (Effect)
import React.Basic.Hooks (Hook, UseEffect, UseState, coerceHook, useEffect, useState, (/\))
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Win

newtype UseKeyUp hooks
  = UseKeyUp
  (UseEffect Unit (UseState Boolean hooks))

derive instance ntUseKeyUp ∷ Newtype (UseKeyUp hooks) _
-- useScrollYPosition ∷ Hook UseScrollYPosition Number
useKeyUp ∷ Int -> Effect Unit -> Hook UseKeyUp Unit
useKeyUp targetKey doWhat = do
  coerceHook React.do
    keyPressed /\ modifyKeyPressed <- useState false
    useEffect unit do
      listener <-
        eventListener \event -> when (getKeyCode event == Just targetKey) doWhat
      win <- window
      addEventListener eventTypeKeyUp listener false (Win.toEventTarget win)
      pure (removeEventListener eventTypeKeyUp listener false (Win.toEventTarget win))

eventTypeKeyUp ∷ EventType
eventTypeKeyUp = EventType "keyup"

foreign import getKeyImpl ∷ ∀ a. (a -> Maybe a) -> Maybe a -> Event -> Maybe Int

getKeyCode ∷ Event -> Maybe Int
getKeyCode = getKeyImpl Just Nothing
