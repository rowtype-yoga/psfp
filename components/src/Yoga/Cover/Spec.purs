module Yoga.Cover.Spec where

import Prelude
import Yoga.Cover.Component as Cover
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)

spec :: Spec Unit
spec =
  describeComponent Cover.makeComponent
    "The Cover Component" do
    it "renders without problems" \panel -> do
      _ <- renderComponent panel (Cover.withDefaults {})
      pure unit
