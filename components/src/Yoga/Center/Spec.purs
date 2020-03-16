module Yoga.Center.Spec where

import Prelude
import Yoga.Center.Component as Center
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)

spec :: Spec Unit
spec =
  describeComponent Center.makeComponent
    "The Center Component" do
    it "renders without problems" \panel -> do
      _ <- renderComponent panel (Center.withDefaults {})
      pure unit
