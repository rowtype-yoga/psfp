module React.Basic.Extra.Hooks.UseAffReducer where

import Prelude
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React.Basic.Hooks (Hook, UseState, coerceHook, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)

newtype UseAffReducer state action hooks
  = UseAffReducer
  ( UseAff (Maybe action) Unit
      ( UseState (Maybe action)
          (UseState state hooks)
      )
  )

derive instance ntUseAffReducer ∷ Newtype (UseAffReducer state action hooks) _
useAffReducer ∷ ∀ state action. Eq action => state -> (state -> action -> Aff state) -> Hook (UseAffReducer state action) (Tuple state (action -> Effect Unit))
useAffReducer initialState reducer =
  coerceHook React.do
    state /\ modifyState <- useState initialState
    maybeAction /\ modifyAction <- useState Nothing
    useAff maybeAction do
      for_ maybeAction \action -> do
        newState <- reducer state action
        liftEffect do
          modifyState (const newState)
          modifyAction (const Nothing)
    let
      dispatch = modifyAction <<< const <<< Just
    pure (state /\ dispatch)
