module Yoga.WithSidebar.Spec where

import Prelude
import Justifill (justifill)
import React.Basic (JSX)
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.WithSidebar.Component as WithSidebar

spec ∷ Spec Unit
spec =
  describeComponent WithSidebar.makeComponent
    "The WithSidebar Component" do
    it "renders without problems" \withSidebar -> do
      _ <-
        renderComponent withSidebar
          $ justifill
              { sidebarChildren: [] ∷ _ JSX
              , notSidebarChildren: [] ∷ _ JSX
              }
      pure unit
