module Yoga.Imposter.Spec where

import Prelude
import Justifill (justifill)
import React.Basic (JSX)
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Imposter.Component as Imposter

spec ∷ Spec Unit
spec =
  describeComponent Imposter.makeComponent
    "The Imposter Component" do
    it "renders without problems" \panel -> do
      _ <- renderComponent panel (justifill { kids: [] ∷ Array JSX })
      pure unit
