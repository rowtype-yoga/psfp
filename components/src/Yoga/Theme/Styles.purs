module Theme.Styles
  ( makeStyles_
  , makeStyles
  , useTheme
  , UseStyles
  , UseTheme
  , classNames
  , unsafeMakeStyles
  ) where

import Prelude
import Data.Foldable (intercalate)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Prim.RowList (class RowToList)
import React.Basic.DOM (CSS)
import React.Basic.Hooks (Hook)
import Record.Extra (class MapRecord)
import Theme.Types (CSSTheme)
import Type.Row.Homogeneous (class Homogeneous)

foreign import data UseStyles ∷ Type -> Type -> Type

foreign import makeStylesImpl ∷
  ∀ css classNames.
  EffectFn1 { | css } (Hook (UseStyles { | css }) { | classNames })

foreign import makeStylesThemedImpl ∷
  ∀ theme css classNames.
  EffectFn1 ({ | theme } -> { | css }) (Hook (UseStyles { | css }) { | classNames })

foreign import unsafeMakeStyles ∷
  ∀ theme css classNames.
  EffectFn1 (theme -> css) (Hook (UseStyles css) { | classNames })

makeStyles ∷
  ∀ theme css cssList classNames.
  RowToList css cssList =>
  MapRecord cssList css CSS String () classNames =>
  ({ | theme } -> { | css }) ->
  Effect (Hook (UseStyles { | css }) { | classNames })
makeStyles = runEffectFn1 makeStylesThemedImpl

makeStyles_ ∷
  ∀ css cssList classNames.
  RowToList css cssList =>
  MapRecord cssList css CSS String () classNames =>
  { | css } ->
  Effect (Hook (UseStyles { | css }) { | classNames })
makeStyles_ = runEffectFn1 makeStylesImpl

foreign import data UseTheme ∷ Type -> Type

foreign import useThemeImpl ∷
  ∀ classNames.
  Hook (UseTheme) { | classNames }

useTheme ∷ Hook (UseTheme) CSSTheme
useTheme = useThemeImpl

classNames ∷ ∀ r. Homogeneous r String => Array (Record r -> String) -> Record r -> String
classNames cs allClasses = (cs <#> \x -> x allClasses) # intercalate " "
