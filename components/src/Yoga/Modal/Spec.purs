module Yoga.Modal.Spec where

import Prelude
import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM as R
import React.TestingLibrary (renderComponent)
import Test.Spec (Spec, describe, itOnly)
import Yoga.Modal.Component as Modal
import Yoga.Spec.Helpers (withDarkTheme)

spec ∷ Spec Unit
spec =
  describe
    "The Modal Component" do
    itOnly "renders without problems" do
      modal <- withDarkTheme Modal.makeComponent
      _ <-
        renderComponent modal
          $ justifill
              { title: "Hey"
              , content: R.text "content"
              , onClose: (pure unit) ∷ Effect Unit
              }
      pure unit
 {- it "calls the onClose handler when clicking away" \panel -> do
       -- [TODO]
       _ <- renderComponent panel (justifill { kids: [], onClose: pure unit })
       pure unit
    -}