module Yoga.Grimoire.Spell.Spec where

import Prelude
import Justifill (justifill)
import React.Basic.DOM as R
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Grimoire.Spell.Component as Spell
import Yoga.Spec.Helpers (withSpecTheme)

spec ∷ Spec Unit
spec =
  describeComponent (withSpecTheme Spell.makeComponent)
    "The Spell Component" do
    it "renders without problems" \spell -> do
      _ <-
        renderComponent spell
          $ justifill { spell: { description: "bla", name: "Heinz", signature: "Any -> Unit" } }
      pure unit
