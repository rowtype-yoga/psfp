module Yoga.Template.Spec where

import Prelude
import Justifill (justifill)
import React.Basic.DOM as R
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Template.Component as Template
import Yoga.Spec.Helpers (withSpecTheme)

spec ∷ Spec Unit
spec =
  describeComponent (withSpecTheme Template.makeComponent)
    "The Template Component" do
    it "renders without problems" \template -> do
      _ <-
        renderComponent template
          $ justifill
              {}
      pure unit
