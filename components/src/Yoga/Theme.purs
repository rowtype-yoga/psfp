module Yoga.Theme where

import Prelude
import CSS (contrast, darken, isLight, lighten, rotateHue, toHexString)
import CSS as Color
import Color (Color)
import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe)
import Data.Symbol (SProxy(..))
import Record.Builder as RB
import Yoga.Theme.Types (Theme, CSSTheme)

increaseContrast ∷ Color -> Color -> Color
increaseContrast contrastWith = go 0
  where
  modify = if isLight contrastWith then darken else lighten
  go i col =
    if contrast contrastWith col >= 7.5 || i >= 20 then
      col
    else
      go (i + 1) (modify 0.1 col)

unsafeAlaColor ∷ (Color -> Color) -> String -> String
unsafeAlaColor fn s1 =
  fromMaybe "hotpink" do
    c1 <- Color.fromHexString s1
    pure $ Color.toHexString (fn c1)

f ∷ ∀ fieldName. SProxy fieldName
f = SProxy

fromTheme ∷ Theme -> CSSTheme
fromTheme theme =
  RB.build
    ( {- background -} RB.modify (f ∷ _ "backgroundColour") toHexString
        >>> RB.insert (f ∷ _ "backgroundColourLighter")
            (theme.backgroundColour # lighter >>> toHexString)
        >>> RB.insert (f ∷ _ "backgroundColourLightest")
            (theme.backgroundColour # lighter # lighter >>> toHexString)
        >>> RB.insert (f ∷ _ "backgroundColourDarker")
            (theme.backgroundColour # darker >>> toHexString)
        >>> RB.insert (f ∷ _ "backgroundColourDarkest")
            (theme.backgroundColour # darker # darker >>> toHexString)
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
        >>> RB.insert (f ∷ _ "highlightColourRotatedForwards")
            (theme.highlightColour # rotateHue (20.0) # toHexString)
        >>> RB.insert (f ∷ _ "highlightColourRotatedBackwards")
            (theme.highlightColour # rotateHue (-20.0) # toHexString)
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
        >>> RB.modify (f ∷ _ "white") toHexString
        -- fonts
        
        >>> RB.modify (f ∷ _ "textFontFamily") (intercalate ",")
        >>> RB.modify (f ∷ _ "headingFontFamily") (intercalate ",")
        >>> RB.modify (f ∷ _ "codeFontFamily") (intercalate ",")
        >>> RB.insert (f ∷ _ "isLight") isLightTheme
        >>> RB.insert (f ∷ _ "fontWeightBold") "300"
    )
    theme
  where
  isLightTheme = isLight theme.backgroundColour
  lighter ∷ Color -> Color
  lighter = if isLightTheme then lighten 0.04 else lighten 0.02
  darker ∷ Color -> Color
  darker = if isLightTheme then darken 0.01 else darken 0.01
