module React.Basic.Hooks.UseGesture where

import Prelude
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Lacks, class Nub)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Hook, unsafeHook)
import Record (disjointUnion)

foreign import data UseDrag ∷ Type -> Type -> Type

type DragHandler
  = { down ∷ Boolean, movement ∷ Tuple Number Number } -> Effect Unit

type DragHandlerImpl
  = { down ∷ Boolean, movement ∷ Tuple Number Number } -> Unit

type DragProps
  = { onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler }

foreign import useDragImpl ∷ EffectFn1 DragHandlerImpl (Effect DragProps)

useDrag ∷ DragHandler -> Hook (UseDrag Unit) (Effect DragProps)
useDrag dragHandler = unsafeHook (runEffectFn1 useDragImpl x)
  where
  x ∷ DragHandlerImpl
  x = unsafePerformEffect <<< dragHandler

withDragProps ∷
  ∀ attrs.
  Lacks "onMouseDown" attrs =>
  Lacks "onTouchStart" attrs =>
  Nub
    ( onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler | attrs )
    ( onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler | attrs ) =>
  { | attrs } ->
  Effect DragProps ->
  { onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler | attrs }
withDragProps x (y ∷ Effect DragProps) = disjointUnion (unsafePerformEffect y) x
