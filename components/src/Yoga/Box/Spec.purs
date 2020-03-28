module Yoga.Box.Spec where

import Prelude
import Justifill (justifill)
import React.Basic.DOM as R
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Box.Component as Box
import Yoga.Spec.Helpers (withDarkTheme)

spec ∷ Spec Unit
spec =
  describeComponent (withDarkTheme Box.makeComponent)
    "The Box Component" do
    it "renders without problems" \stack -> do
      _ <-
        renderComponent stack
          ( justifill
              { kids:
                [ R.h1_ [ R.text "Living in a Box" ]
                , R.h2_ [ R.text "Living in a Card, Board, Box" ]
                ]
              , padding: "12rem"
              }
          )
      pure unit
