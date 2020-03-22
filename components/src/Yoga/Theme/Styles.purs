module Yoga.Theme.Styles
  ( useTheme
  , UseStyles
  , UseTheme
  , classNames
  , makeStylesJSS
  ) where

import Prelude
import Data.Foldable (intercalate)
import Effect (Effect)
import Foreign (Foreign)
import JSS (JSSClasses(..), JSSElem)
import Prim.RowList (class RowToList)
import React.Basic.Hooks (Hook)
import Record.Extra (class MapRecord, mapRecord)
import Simple.JSON as Foreign
import Type.Row.Homogeneous (class Homogeneous)
import Yoga.Theme.Types (CSSTheme)

foreign import data UseStyles ∷ Type -> Type -> Type

foreign import makeStylesWithPropsImpl ∷
  ∀ css classNames theme props.
  ({ | theme } -> { | css }) ->
  Effect (props -> (Hook (UseStyles props) { | classNames }))

makeStylesJSS ∷
  ∀ theme jss jssRL jssForeign classes props.
  RowToList jss jssRL =>
  MapRecord jssRL jss (JSSElem props) Foreign () jssForeign =>
  MapRecord jssRL jss (JSSElem props) String () classes =>
  JSSClasses theme props jss ->
  Effect (props -> Hook (UseStyles props) { | classes })
makeStylesJSS (JSSClasses themeToJssClasses) =
  makeStylesWithPropsImpl
    ( themeToJssClasses <#> mapRecord Foreign.write
    )

foreign import data UseTheme ∷ Type -> Type

foreign import useThemeImpl ∷
  ∀ classNames.
  Hook (UseTheme) { | classNames }

useTheme ∷ Hook (UseTheme) CSSTheme
useTheme = useThemeImpl

classNames ∷ ∀ r. Homogeneous r String => Array (Record r -> String) -> Record r -> String
classNames cs allClasses = (cs <#> \f -> f allClasses) # intercalate " "
