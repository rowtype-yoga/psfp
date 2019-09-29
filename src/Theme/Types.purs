module Theme.Types where

import Prelude

import CSS (darken, lighten)
import CSS.Color (toHexString)
import Color (Color)
import Data.Foldable (intercalate)
import Data.Function (applyFlipped)
import Data.Symbol (SProxy(..))
import Record.Builder as RB

type Theme =
  { backgroundColour ∷ Color
  , foregroundColour ∷ Color
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
        (theme.backgroundColour # lighten 0.2 # toHexString)
  >>> RB.insert (f ∷ _ "backgroundColourLight")
        (theme.backgroundColour # lighten 0.4 # toHexString)
  >>> RB.insert (f ∷ _ "backgroundColourDarker")
        (theme.backgroundColour # darken 0.2 # toHexString)
  >>> RB.insert (f ∷ _ "backgroundColourDark")
        (theme.backgroundColour # darken 0.4 # toHexString)
  -- foreground
  >>> RB.modify (f ∷ _ "foregroundColour") toHexString
  >>> RB.insert (f ∷ _ "foregroundColourLighter")
        (theme.foregroundColour # lighten 0.2 # toHexString)
  >>> RB.insert (f ∷ _ "foregroundColourLight")
        (theme.foregroundColour # lighten 0.4 # toHexString)
  >>> RB.insert (f ∷ _ "foregroundColourDarker")
        (theme.foregroundColour # darken 0.2 # toHexString)
  >>> RB.insert (f ∷ _ "foregroundColourDark")
        (theme.foregroundColour # darken 0.4 # toHexString)
  -- highlight
  >>> RB.modify (f ∷ _ "highlightColour") toHexString
  >>> RB.insert (f ∷ _ "highlightColourDark")
        (theme.highlightColour # darken 0.4 # toHexString)
  -- altHighlight
  >>> RB.modify (f ∷ _ "altHighlightColour") toHexString
  >>> RB.insert (f ∷ _ "altHighlightColourDark")
        (theme.altHighlightColour # darken 0.4 # toHexString)
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
  ) theme

type CSSTheme =
  { backgroundColour ∷ String
  , backgroundColourDarker ∷ String
  , backgroundColourDark ∷ String
  , backgroundColourLighter ∷ String
  , backgroundColourLight ∷ String
  , foregroundColourLight ∷ String
  , foregroundColourLighter ∷ String
  , foregroundColour ∷ String
  , foregroundColourDarker ∷ String
  , foregroundColourDark ∷ String
  , highlightColour ∷ String
  , highlightColourDark ∷ String
  , altHighlightColour ∷ String
  , altHighlightColourDark ∷ String
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

