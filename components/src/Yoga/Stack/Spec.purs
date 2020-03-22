module Yoga.Stack.Spec where

import Prelude
import Justifill (justifill)
import React.Basic.DOM as R
import React.TestingLibrary (describeComponent, renderComponent)
import Test.Spec (Spec, it)
import Yoga.Stack.Component as Stack

spec ∷ Spec Unit
spec =
  describeComponent Stack.makeComponent
    "The Stack Component" do
    it "renders without problems" \stack -> do
      _ <-
        renderComponent stack
          ( justifill
              { kids:
                [ R.h1_ [ R.text "We are stacked" ]
                ]
              , space: "4rem"
              , splitAfter: 7
              }
          )
      pure unit
