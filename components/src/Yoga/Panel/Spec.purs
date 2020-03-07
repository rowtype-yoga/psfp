module Panel.Spec where

import Prelude
import Yoga.Panel.Component as Panel
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)

spec :: Spec Unit
spec =
  describeComponent Panel.makeComponent
    "The Panel Component" do
    it "renders without problems" \panel -> do
      _ <- renderComponent panel (Panel.withDefaults {})
      pure unit
