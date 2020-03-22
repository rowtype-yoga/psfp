module Yoga.Centre.Spec where

import Prelude
import Justifill (justifill)
import React.Basic.DOM as R
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Centre.Component as Centre

spec ∷ Spec Unit
spec =
  describeComponent Centre.makeComponent
    "The Centre Component" do
    it "renders without problems" \panel -> do
      _ <- renderComponent panel (justifill { kids: [ R.text "Child" ], andText: true })
      pure unit
