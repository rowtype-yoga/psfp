module Yoga.CloseIcon.Spec where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Justifill (justifill)
import React.TestingLibrary (describeComponent, fireEventClick, renderComponent)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.CloseIcon.Component as CloseIcon
import Yoga.Spec.Helpers (withSpecTheme)

spec ∷ Spec Unit
spec =
  describeComponent (withSpecTheme CloseIcon.makeComponent)
    "The CloseIcon Component" do
    it "renders without problems" \closeicon -> do
      _ <-
        renderComponent closeicon
          $ justifill
              { onClick: (pure unit) ∷ Effect Unit
              }
      pure unit
    it "calls the onClose handler when clicking on the svg close" \closeicon -> do
      ref <- Ref.new false # liftEffect
      { findByTestId } <-
        renderComponent closeicon
          $ justifill
              { onClick: Ref.write true ref
              }
      closeBtn <- findByTestId "close-icon-svg"
      fireEventClick closeBtn
      clicked <- Ref.read ref # liftEffect
      clicked `shouldEqual` true
