module Yoga.CSS.Safer where

import React.Basic.DOM (CSS, css)
import Simple.JSON (class WriteForeign)

cssSafer ∷ ∀ r. WriteForeign { | r } => { | r } -> CSS
cssSafer = css
