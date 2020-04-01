module React.Basic.Hooks.Spring where

import Effect.Uncurried (EffectFn1, runEffectFn1)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (CSS, Props_div)
import React.Basic.Hooks (Hook, unsafeHook)

foreign import data UseSpring ∷ Type -> Type -> Type

foreign import data SpringStyles ∷ Type

foreign import useSpringImpl ∷ EffectFn1 CSS SpringStyles

useSpring ∷ CSS -> Hook (UseSpring CSS) SpringStyles
useSpring css' = unsafeHook (runEffectFn1 useSpringImpl css')

foreign import animatedImpl ∷ ∀ attrs. String -> ReactComponent { | attrs }

foreign import animatedComponentImpl ∷ ∀ attrs. ReactComponent { style ∷ CSS | attrs } -> ReactComponent { style ∷ SpringStyles | attrs }

animated ∷
  ∀ attrs.
  ReactComponent { style ∷ CSS | attrs } ->
  ReactComponent
    { style ∷ SpringStyles
    | attrs
    }
animated = animatedComponentImpl

animatedDiv ∷
  ∀ attrs attrs_.
  Union attrs attrs_ Props_div =>
  { style ∷ SpringStyles | attrs } ->
  JSX
animatedDiv = element (animatedImpl "div")
