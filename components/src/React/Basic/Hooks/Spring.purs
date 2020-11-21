module React.Basic.Hooks.Spring where

import Prelude
import Data.Function.Uncurried (Fn4, mkFn4)
import Effect (Effect)
import Effect.Aff.Compat (runEffectFn2)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (CSS, Props_div, Props_span)
import React.Basic.DOM.SVG (Props_svg, Props_path)
import React.Basic.Hooks (Hook, unsafeHook)
import React.Basic.Hooks as React

foreign import data UseSpring ∷ Type -> Type -> Type

type StopSpring
  = Effect Unit

type SetSpringImpl r
  = EffectFn1 { | r } Unit

type SetSpring r
  = { | r } -> Effect Unit

foreign import useSpringImpl ∷ ∀ r s props. (props -> { | r }) -> Effect { style ∷ { | r }, set ∷ SetSpringImpl s, stop ∷ StopSpring }

useSpring ∷ ∀ r s props. (props -> { | r }) -> Hook (UseSpring { | r }) { style ∷ { | r }, set ∷ SetSpring s, stop ∷ StopSpring }
useSpring f = React.do
  res <- unsafeHook (useSpringImpl f)
  pure $ res { set = runEffectFn1 res.set }

foreign import data UseSprings ∷ Type -> Type -> Type

foreign import useSpringsImpl ∷
  ∀ r s.
  Int ->
  (Int -> { | r }) ->
  Effect { styles ∷ Array { | r }, set ∷ SetSpringsImpl s, stop ∷ StopSpring }

type SetSpringsImpl r
  = EffectFn1 (Int -> { | r }) Unit

type SetSprings r
  = (Int -> { | r }) -> Effect Unit

useSprings ∷
  ∀ r s.
  Int ->
  (Int -> { | r }) -> Hook (UseSprings { | r }) { styles ∷ Array { | r }, set ∷ SetSprings s, stop ∷ StopSpring }
useSprings n f = React.do
  res <- unsafeHook (useSpringsImpl n f)
  pure $ res { set = runEffectFn1 res.set }

foreign import data UseTransition ∷ Type -> Type -> Type

type TransitionFnImpl a props
  = (Fn4 { | props } a (TransitionObjectImpl a) Int JSX) -> JSX

foreign import data Controller ∷ Type

type TransitionObjectImpl a
  = { key ∷ String, item ∷ a, phase ∷ String, ctrl ∷ Controller }

type TransitionObject a
  = { key ∷ String, item ∷ a, phase ∷ TransitionPhase, ctrl ∷ Controller }

type TransitionFn a props
  = ({ | props } -> a -> TransitionObject a -> Int -> JSX) -> JSX

data TransitionPhase
  = MountPhase
  | EnterPhase
  | UpdatePhase
  | LeavePhase

transitionPhaseFromString ∷ String -> TransitionPhase
transitionPhaseFromString string =
  unsafePartial case string of
    "mount" -> MountPhase
    "enter" -> EnterPhase
    "update" -> UpdatePhase
    "leave" -> LeavePhase

foreign import useTransitionImpl ∷ ∀ a props. EffectFn2 (Array a) { | props } (TransitionFnImpl a props)

useTransition ∷
  ∀ item props.
  Array item ->
  { | props } ->
  Hook (UseTransition item) (TransitionFn item props)
useTransition items props = unsafeHook $ transitionFn
  where
  transitionFn ∷ Effect (TransitionFn item props)
  transitionFn = ado
    transitionFnImpl <- runEffectFn2 useTransitionImpl items props
    in \f -> mkFn4 (transitionFnToTransitionFnImpl f) # transitionFnImpl

transitionFnToTransitionFnImpl ∷
  ∀ item props.
  (_ -> _ -> TransitionObject item -> _ -> JSX) ->
  _ -> _ -> TransitionObjectImpl item -> _ -> JSX
transitionFnToTransitionFnImpl f = \a b c -> f a b (c { phase = transitionPhaseFromString c.phase })

foreign import animatedImpl ∷ ∀ attrs. String -> ReactComponent { | attrs }

foreign import animatedComponentImpl ∷ ∀ attrs. ReactComponent { style ∷ CSS | attrs } -> ReactComponent { style ∷ CSS | attrs }

animated ∷
  ∀ attrs.
  ReactComponent { style ∷ CSS | attrs } ->
  ReactComponent
    { style ∷ CSS
    | attrs
    }
animated = animatedComponentImpl

animatedDiv ∷
  ∀ attrs attrs_.
  Union attrs attrs_ Props_div =>
  { style ∷ CSS | attrs } ->
  JSX
animatedDiv = element (animatedImpl "div")

animatedSpan ∷
  ∀ attrs attrs_.
  Union attrs attrs_ Props_span =>
  { style ∷ CSS | attrs } ->
  JSX
animatedSpan = element (animatedImpl "span")

animatedSvg ∷
  ∀ attrs attrs_.
  Union attrs attrs_ Props_svg =>
  { style ∷ CSS | attrs } ->
  JSX
animatedSvg = element (animatedImpl "svg")

animatedPath ∷
  ∀ attrs attrs_.
  Union attrs attrs_ Props_path =>
  { style ∷ CSS | attrs } ->
  JSX
animatedPath = element (animatedImpl "path")
