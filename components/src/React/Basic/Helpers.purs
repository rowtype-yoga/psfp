module React.Basic.Helpers where

import Justifill (justifill)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import React.Basic (JSX)
import React.Basic.Hooks (ReactComponent, element)

element_ ::
  forall requiredProps filledProps suppliedProps missingProps missingPropsRow justifiableRow.
  RowToList missingProps missingPropsRow =>
  FillableFields missingPropsRow () missingProps =>
  Union filledProps missingProps requiredProps =>
  Nub requiredProps requiredProps =>
  RowToList suppliedProps justifiableRow =>
  JustifiableFields justifiableRow suppliedProps () filledProps =>
  ReactComponent (Record requiredProps) ->
  Record suppliedProps ->
  JSX
element_ x props = element x (justifill props)
