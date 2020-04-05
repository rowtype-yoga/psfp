module Yoga.Modal.Spec where

import Prelude
import Justifill (justifill)
import React.Basic.DOM as R
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Modal.Component as Modal
import Yoga.Spec.Helpers (withDarkTheme)

spec ∷ Spec Unit
spec =
  describeComponent (withDarkTheme Modal.makeComponent)
    "The Modal Component" do
    it "renders without problems" \modal -> do
      _ <-
        renderComponent modal
          $ justifill
              { title: "Hey"
              , kids: [ R.text "content" ]
              }
      pure unit
