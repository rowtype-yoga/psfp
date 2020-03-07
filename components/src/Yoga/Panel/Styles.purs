module Yoga.Panel.Styles where

import Prelude
import Yoga.CSS.Safer (cssSafer)
import Data.Interpolate (i)
import React.Basic.DOM (CSS)
import Yoga.Theme.Types (CSSTheme)

styles ::
  CSSTheme ->
  { panel :: CSS
  }
styles theme =
  { panel:
    cssSafer
      { background: theme.backgroundColour
      , borderRight: "1px solid " <> theme.backgroundColourLightest
      , color: theme.textColour
      , fontFamily: theme.textFontFamily
      , fontSize: "1em"
      , padding: "20px"
      , display: "flex"
      , flexDirection: "column"
      , height: "100%"
      , boxShadow:
        (i "30px 30px 60px " theme.backgroundColourDarker ", -30px -30px 60px " theme.backgroundColourLighter ";") :: String
      }
  }
