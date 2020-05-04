module Yoga.Cover.Spec where

import Prelude
import Justifill (justifill)
import React.Basic.DOM as R
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Cover.Component as Cover

spec ∷ Spec Unit
spec =
  describeComponent Cover.makeComponent
    "The Cover Component" do
    it "renders without problems" \cover -> do
      _ <- renderComponent cover (justifill { kids: [ R.text "hi" ] })
      pure unit
