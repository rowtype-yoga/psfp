module Gatsby where

import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic.Hooks (Hook, unsafeHook)

data GraphQlQuery result = GraphQlQuery result

foreign import useStaticQueryImpl ::
  forall a.
  EffectFn1
    (GraphQlQuery a)
    a

foreign import data UseStaticQuery :: Type -> Type -> Type

useStaticQuery ::
  forall a.
  GraphQlQuery a ->
  Hook (UseStaticQuery a) (a)
useStaticQuery query =
  unsafeHook do
    runEffectFn1 useStaticQueryImpl query