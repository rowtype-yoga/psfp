module Yoga.Font where

import React.Basic.DOM (CSS)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Font :: Type

type FontFamily
  = { fontFamily :: String
    , fontStyle :: String
    , fontWeight :: String
    , src :: String
    }

arrayToCss :: Array FontFamily -> CSS
arrayToCss = unsafeCoerce
