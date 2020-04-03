module React.Basic.Hooks.UseGesture where

import Prelude
import Data.Array ((!!))
import Data.Foldable (fold)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Lacks, class Nub)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Hook, unsafeHook)
import Record (disjointUnion)
import Yoga.Helpers ((?||))

foreign import data UseDrag ∷ Type -> Type -> Type

type DragHandler
  = { down ∷ Boolean, movement ∷ Number /\ Number } -> Effect Unit

type DragHandlerImpl
  = { down ∷ Boolean, movement ∷ Array Number } -> Unit

type DragProps
  = { onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler }

foreign import useDragImpl ∷ EffectFn1 DragHandlerImpl (Effect DragProps)

useDrag ∷ DragHandler -> Hook (UseDrag Unit) (Effect DragProps)
useDrag dragHandler = unsafeHook (runEffectFn1 useDragImpl f)
  where
  f x =
    (unsafePerformEffect <<< dragHandler)
      { down: x.down, movement: mx /\ my
      }
    where
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
  Effect DragProps ->
  { onMouseDown ∷ EventHandler, onTouchStart ∷ EventHandler | attrs }
withDragProps x (y ∷ Effect DragProps) = disjointUnion (unsafePerformEffect y) x
