module Yoga.Grid.Spec where

import Prelude
import Justifill (justifill)
import React.Basic.DOM as R
import React.Basic.Hooks (JSX)
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Grid.Component as Grid
import Yoga.Spec.Helpers (withSpecTheme)

spec ∷ Spec Unit
spec =
  describeComponent (withSpecTheme Grid.makeComponent)
    "The Grid Component" do
    it "renders without problems" \grid -> do
      _ <-
        renderComponent grid
          $ justifill
              { kids: [] ∷ _ JSX }
      pure unit
