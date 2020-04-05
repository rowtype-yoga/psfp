module Yoga.ClickAway.Spec where

import Prelude
import Effect (Effect)
import Justifill (justifill)
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.ClickAway.Component as ClickAway
import Yoga.Spec.Helpers (withDarkTheme)

spec ∷ Spec Unit
spec =
  describeComponent (withDarkTheme ClickAway.makeComponent)
    "The ClickAway Component" do
    it "renders without problems" \clickaway -> do
      _ <-
        renderComponent clickaway
          $ justifill
              { onClick: (pure unit) ∷ Effect Unit
              }
      pure unit
