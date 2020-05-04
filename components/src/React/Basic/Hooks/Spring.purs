module React.Basic.Hooks.Spring where

import Prelude
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
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

foreign import useTransitionImpl ∷ ∀ a. EffectFn3 (Array a) (Nullable (a -> String)) CSS (Array { item ∷ Nullable a, key ∷ Nullable String, props ∷ CSS })

useTransition ∷
  ∀ item.
  Array item ->
  Maybe (item -> String) ->
  CSS ->
  Hook (UseTransition item) (Array { item ∷ Maybe item, key ∷ Maybe String, props ∷ CSS })
useTransition items toKey transition =
  unsafeHook $ runEffectFn3 useTransitionImpl items (toNullable toKey) transition
    <#> map \x@{ item, key } ->
        x
          { item = item # Nullable.toMaybe
          , key = key # Nullable.toMaybe
          }

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
