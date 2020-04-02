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

foreign import useSpringImpl ∷ ∀ r props. _ -> (props -> { | r }) -> Effect ({ | r } /\ (EffectFn1 { | r } Unit))

useSpring ∷ ∀ r props. (props -> { | r }) -> Hook (UseSpring { | r }) ({ | r } /\ ({ | r } -> Effect Unit))
useSpring f = React.do
  styles /\ setFn <- unsafeHook (useSpringImpl Tuple (f))
  pure (styles /\ (runEffectFn1 setFn))

foreign import data UseTransition ∷ Type -> Type -> Type

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
