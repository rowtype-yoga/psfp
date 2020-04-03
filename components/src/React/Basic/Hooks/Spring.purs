module React.Basic.Hooks.Spring where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (CSS, Props_div)
import React.Basic.Hooks (Hook, unsafeHook)
import React.Basic.Hooks as React

foreign import data UseSpring ∷ Type -> Type -> Type

type StopSpring
  = Effect Unit

type SetSpringImpl r
  = EffectFn1 { | r } Unit

type SetSpring r
  = { | r } -> Effect Unit

foreign import useSpringImpl ∷ ∀ r props. (props -> { | r }) -> Effect { style ∷ { | r }, set ∷ SetSpringImpl r, stop ∷ StopSpring }

useSpring ∷ ∀ r props. (props -> { | r }) -> Hook (UseSpring { | r }) { style ∷ { | r }, set ∷ SetSpring r, stop ∷ StopSpring }
useSpring f = React.do
  res <- unsafeHook (useSpringImpl f)
  pure $ res { set = runEffectFn1 res.set }

foreign import data UseTransition ∷ Type -> Type -> Type

{- foreign import useTransitionImpl ∷ ∀ r. _ -> () -> Effect ({ | r } /\ (EffectFn1 { | r } Unit))

-- useTransition ∷ ∀ r props. (props -> { | r }) -> Hook (UseTransition { | r }) ({ | r } /\ ({ | r } -> Effect Unit))
-- useTransition f = React.do
--   styles /\ setFn <- unsafeHook (useTransitionImpl Tuple (f))
--   pure (styles /\ (runEffectFn1 setFn))
-}
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
