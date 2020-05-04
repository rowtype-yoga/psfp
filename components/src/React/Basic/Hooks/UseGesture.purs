module React.Basic.Hooks.UseGesture where

import Prelude
import Data.Array ((!!))
import Data.Maybe (Maybe, fromMaybe')
import Data.Nullable (Nullable)
import Data.Tuple.Nested ((/\), type (/\))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff.Compat (runEffectFn2)
import Effect.Uncurried (EffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Lacks, class Nub)
import React.Basic.Events (EventHandler)
import React.Basic.Helpers (orUndefined)
import React.Basic.Hooks (Hook, Ref, unsafeHook)
import Record (disjointUnion)
import Web.DOM (Node)
import Yoga.Helpers ((?||))

foreign import data UseDrag ∷ Type -> Type -> Type

type DragHandler a
  = { arg ∷ a, down ∷ Boolean, movement ∷ Number /\ Number } -> Effect Unit

type DragHandlerImpl a
  = { args ∷ Array a, down ∷ Boolean, movement ∷ Array Number } -> Unit

type DragProps
  = { onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler }

type GenericOptionsImpl r
  = { domTarget ∷ Ref (Nullable Node) | r }

type GenericOptions r
  = { domTarget ∷ Maybe (Ref (Nullable Node)) | r }

type DragOptions
  = GenericOptions ()

type DragOptionsImpl
  = GenericOptionsImpl ()

dragOptionsToImpl ∷ DragOptions -> DragOptionsImpl
dragOptionsToImpl { domTarget } = { domTarget: domTarget # orUndefined }

foreign import useDragImpl ∷ ∀ a. EffectFn2 (DragHandlerImpl a) DragOptionsImpl (a -> DragProps)

useDrag ∷ ∀ a. DragOptions -> DragHandler a -> Hook (UseDrag Unit) (a -> DragProps)
useDrag dragOptions dragHandler = unsafeHook (runEffectFn2 useDragImpl dragHandlerImpl (dragOptionsToImpl dragOptions))
  where
  dragHandlerImpl ∷ DragHandlerImpl a
  dragHandlerImpl x =
    (unsafePerformEffect <<< dragHandler)
      { arg, down: x.down, movement: mx /\ my
      }
    where
    arg = x.args !! 0 # fromMaybe' (\_ -> unsafeCrashWith "Bollox")
    mx = x.movement !! 0 ?|| 0.0
    my = x.movement !! 1 ?|| 0.0

withDragProps ∷
  ∀ attrs.
  Lacks "onMouseDown" attrs =>
  Lacks "onTouchStart" attrs =>
  Nub
    ( onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler | attrs )
    ( onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler | attrs ) =>
  { | attrs } ->
  DragProps ->
  { onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler | attrs }
withDragProps x (y ∷ DragProps) = disjointUnion y x
