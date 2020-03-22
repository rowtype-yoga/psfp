module Yoga.Cluster.Spec where

import Prelude
import Justifill (justifill)
import React.Basic (JSX)
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Cluster.Component as Cluster

spec ∷ Spec Unit
spec =
  describeComponent Cluster.makeComponent
    "The Cluster Component" do
    it "renders without problems" \panel -> do
      _ <- renderComponent panel (justifill { kids: [] ∷ Array JSX })
      pure unit
