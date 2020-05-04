module React.Basic.Helpers where

import Prelude
import Data.Maybe (Maybe, fromMaybe')
import Data.Symbol (SProxy(..))
import Justifill.Fillable (class Fillable, class FillableFields, fill)
import Justifill.Justifiable (class Justifiable, justify)
import Literals.Undefined (undefined)
import Prim.Row (class Lacks, class Nub)
import Prim.RowList (class RowToList)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, element)
import Record (disjointUnion, insert)
import Unsafe.Coerce (unsafeCoerce)

element_ ∷
  ∀ to missing missingRL thru thruRL from fromRL.
  RowToList missing missingRL =>
  RowToList from fromRL =>
  RowToList thru thruRL =>
  Justifiable { | from } { | thru } =>
  Fillable { | thru } { | to } =>
  FillableFields fromRL missing to =>
  -- arguments
  ReactComponent (Record to) ->
  Record from ->
  JSX
element_ x partialProps = element x props
  where
  props ∷ Record to
  props =
    (fill ∷ { | thru } -> { | to })
      ( (justify ∷ { | from } -> { | thru })
          partialProps
      )

type Kids r
  = ( kids ∷ Array JSX | r )

bespoke ∷ ∀ r. Lacks "kids" r => Record r -> Array JSX -> { | Kids r }
bespoke rec kids = insert (SProxy ∷ SProxy "kids") kids rec

infixr 12 bespoke as @@

jsx ∷
  ∀ to thru from.
  Lacks "kids" from =>
  Lacks "kids" to =>
  Justifiable { | from } { | thru } =>
  Fillable { | thru } { | to } =>
  Nub (Kids to) (Kids to) =>
  -- arguments
  ReactComponent (Record (Kids to)) ->
  Record from ->
  Array JSX ->
  JSX
jsx x partialProps kids = element x propsWithKids
  where
  propsWithKids ∷ Record (Kids to)
  propsWithKids = disjointUnion { kids } props
  fill' ∷ { | thru } -> { | to }
  fill' = fill
  justify' ∷ { | from } -> { | thru }
  justify' = justify
  props ∷ Record to
  props = fill' (justify' partialProps)

orUndefined ∷ ∀ a. Maybe a -> a
orUndefined = fromMaybe' \_ -> unsafeCoerce undefined

classSpan ∷ String -> Array JSX -> JSX
classSpan className children = R.span { className, children }

classDiv ∷ String -> Array JSX -> JSX
classDiv className children = R.div { className, children }
