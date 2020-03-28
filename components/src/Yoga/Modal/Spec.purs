module Yoga.Modal.Spec where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Justifill (justifill)
import React.Basic.DOM as R
import React.TestingLibrary (describeComponent, fireEventClick, renderComponent)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
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
              , content: R.text "content"
              , onClose: (pure unit) ∷ Effect Unit
              }
      pure unit
    it "calls the onClose handler when clicking on the svg close" \modal -> do
      ref <- Ref.new false # liftEffect
      { findByTestId } <-
        renderComponent modal
          $ justifill
              { title: "Hey"
              , onClose: Ref.write true ref
              , content: R.text "content"
              }
      closeBtn <- findByTestId "close-icon-svg"
      fireEventClick closeBtn
      clicked <- Ref.read ref # liftEffect
      clicked `shouldEqual` true
