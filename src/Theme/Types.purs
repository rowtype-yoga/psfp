module Theme.Types where

import Prelude

import CSS (ColorSpace(..), blue, brightness, darken, desaturate, isLight, lighten, mix, rotateHue)
import CSS.Color (toHexString)
import Color (Color)
import Data.Foldable (intercalate)
import Data.Symbol (SProxy(..))
import Record.Builder as RB

type Theme =
  { backgroundColour ∷ Color
  , textColour ∷ Color
  , interfaceColour ∷ Color
  , highlightColour ∷ Color
  , altHighlightColour ∷ Color
  , textFontFamily ∷ Array String
  , headingFontFamily ∷ Array String
  , yellow ∷ Color
  , green ∷ Color
  , pink ∷ Color
  , orange ∷ Color
  , turquoise ∷ Color
  , red ∷ Color
  , blue ∷ Color
  }

f ∷ ∀ fieldName. SProxy fieldName
f = SProxy

fromTheme ∷ Theme -> CSSTheme
fromTheme theme = RB.build (
  -- background
      RB.modify (f ∷ _ "backgroundColour") toHexString
  >>> RB.insert (f ∷ _ "backgroundColourLighter")
        (theme.backgroundColour # lighter >>> toHexString)
  >>> RB.modify (f ∷ _ "interfaceColour") toHexString
  >>> RB.insert (f ∷ _ "interfaceColourLighter")
        (theme.interfaceColour # lighter >>> toHexString)
  >>> RB.insert (f ∷ _ "interfaceColourLightest")
        (theme.interfaceColour # lighter >>> lighter >>> toHexString)
  >>> RB.insert (f ∷ _ "interfaceColourDarker")
        (theme.interfaceColour # darker >>> toHexString)
  >>> RB.insert (f ∷ _ "interfaceColourDarkest")
        (theme.interfaceColour # darker >>> darker >>> toHexString)
  -- foreground
  >>> RB.modify (f ∷ _ "textColour") toHexString
  >>> RB.insert (f ∷ _ "textColourLighter")
        (theme.textColour # lighter >>> toHexString)
  >>> RB.insert (f ∷ _ "textColourLightest")
        (theme.textColour # lighter >>> lighter >>> toHexString)
  >>> RB.insert (f ∷ _ "textColourDarker")
        (theme.textColour # darker >>> toHexString)
  >>> RB.insert (f ∷ _ "textColourDarkest")
        (theme.textColour # darker >>> darker >>> toHexString)
  -- highlight
  >>> RB.modify (f ∷ _ "highlightColour") toHexString
  >>> RB.insert (f ∷ _ "highlightColourDark")
        (theme.highlightColour # darken 0.2 # rotateHue (-10.0) # toHexString)
  -- altHighlight
  >>> RB.modify (f ∷ _ "altHighlightColour") toHexString
  >>> RB.insert (f ∷ _ "altHighlightColourDark")
        (theme.altHighlightColour # darken 0.2 # toHexString)
  -- palette
  >>> RB.modify (f ∷ _ "yellow") toHexString
  >>> RB.modify (f ∷ _ "green") toHexString
  >>> RB.modify (f ∷ _ "pink") toHexString
  >>> RB.modify (f ∷ _ "orange") toHexString
  >>> RB.modify (f ∷ _ "turquoise") toHexString
  >>> RB.modify (f ∷ _ "red") toHexString
  >>> RB.modify (f ∷ _ "blue") toHexString
  -- fonts
  >>> RB.modify (f ∷ _ "textFontFamily") (intercalate ",")
  >>> RB.modify (f ∷ _ "headingFontFamily") (intercalate ",")
  >>> RB.insert (f ∷ _ "isLight") isLightTheme
  ) theme
  where
  isLightTheme = isLight theme.backgroundColour
  yellower ∷ Color -> Color
  yellower col =
      if brightness col > 0.8
      then
        lighten 0.02 col
      else
      col
        # lighten 0.1
        # desaturate 0.03

  bluer col =
      mix HSL col blue 0.05
        # darken 0.1
        # desaturate 0.03

  lighter ∷ Color -> Color
  lighter = yellower

  darker ∷ Color -> Color
  darker = bluer

type CSSTheme =
  { backgroundColour ∷ String
  , backgroundColourLighter ∷ String
  , interfaceColour ∷ String
  , interfaceColourDarker ∷ String
  , interfaceColourDarkest ∷ String
  , interfaceColourLighter ∷ String
  , interfaceColourLightest ∷ String
  , textColourLightest ∷ String
  , textColourLighter ∷ String
  , textColour ∷ String
  , textColourDarker ∷ String
  , textColourDarkest ∷ String
  , highlightColour ∷ String
  , highlightColourDark ∷ String
  , altHighlightColour ∷ String
  , altHighlightColourDark ∷ String
  , isLight ∷ Boolean
  , textFontFamily ∷ String
  , headingFontFamily ∷ String
  , yellow ∷ String
  , green ∷ String
  , pink ∷ String
  , orange ∷ String
  , turquoise ∷ String
  , red ∷ String
  , blue ∷ String
  }

