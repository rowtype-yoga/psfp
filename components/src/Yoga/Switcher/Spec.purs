module Yoga.Switcher.Spec where

import Prelude
import Justifill (justifill)
import React.Basic (JSX)
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Switcher.Component as Switcher

spec ∷ Spec Unit
spec =
  describeComponent Switcher.makeComponent
    "The Switcher Component" do
    it "renders without problems" \panel -> do
      _ <- renderComponent panel (justifill { kids: [] ∷ _ JSX, limit: 3 })
      pure unit
