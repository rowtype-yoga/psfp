module Yoga.Theme where

import Prelude
import CSS (CSSVariable, Size, contrast, darken, desaturate, isLight, lighten, reference, rotateHue, unitless, variable, (!*))
import CSS.Size (Calc, (!/))
import Color (Color)
import Color as Color
import Data.Maybe (fromMaybe)
import Data.Symbol (SProxy(..))
import Record.Builder as RB
import Yoga.Theme.Types (Theme, CSSTheme)

increaseContrastTo ∷ Number -> Color -> Color -> Color
increaseContrastTo target contrastWith = go 0
  where
  modify x = if isLight contrastWith then (darken x <<< desaturate (x * (-1.5))) else (lighten x <<< desaturate (x * 0.5))
  go i col =
    if contrast contrastWith col >= ((if isLight contrastWith then (target * 0.8) else target)) || i >= 100 then
      col
    else
      go (i + 1) (modify 0.005 col)

withAlpha ∷ Number -> Color -> Color
withAlpha alpha c1 = Color.rgba' r g b alpha
  where
  { r, g, b } = Color.toRGBA' c1

unsafeWithAlpha ∷ Number -> String -> String
unsafeWithAlpha alpha s1 =
  fromMaybe "yellow" do
    c1 <- Color.fromHexString s1
    let
      { r, g, b } = Color.toRGBA' c1
    pure $ Color.cssStringRGBA (Color.rgba' r g b alpha)

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
    ( {- background -} RB.insert (f ∷ _ "backgroundColourLighter")
        (theme.backgroundColour # lighter)
        >>> RB.insert (f ∷ _ "backgroundColourLightest")
            (theme.backgroundColour # lighter # lighter)
        >>> RB.insert (f ∷ _ "backgroundColourDarker")
            (theme.backgroundColour # darkerBackground)
        >>> RB.insert (f ∷ _ "backgroundColourDarkest")
            (theme.backgroundColour # darkerBackground # darkerBackground)
        >>> RB.insert (f ∷ _ "interfaceColourLighter")
            (theme.interfaceColour # lighter)
        >>> RB.insert (f ∷ _ "interfaceColourLightest")
            (theme.interfaceColour # lighter >>> lighter)
        >>> RB.insert (f ∷ _ "interfaceColourDarker")
            (theme.interfaceColour # darker)
        >>> RB.insert (f ∷ _ "interfaceColourDarkest")
            (theme.interfaceColour # darker >>> darker)
        -- foreground
        
        >>> RB.insert (f ∷ _ "textColourLighter")
            (theme.textColour # lighter)
        >>> RB.insert (f ∷ _ "textColourLightest")
            (theme.textColour # lighter >>> lighter)
        >>> RB.insert (f ∷ _ "textColourDarker")
            (theme.textColour # darker)
        >>> RB.insert (f ∷ _ "textColourDarkest")
            (theme.textColour # darker >>> darker)
        -- highlight
        
        >>> RB.insert (f ∷ _ "highlightColourDark")
            (theme.highlightColour # darken 0.2 # desaturate 0.35)
        >>> RB.insert (f ∷ _ "highlightColourRotatedForwards")
            (theme.highlightColour # rotateHue (30.0))
        >>> RB.insert (f ∷ _ "highlightColourRotatedBackwards")
            (theme.highlightColour # rotateHue (-30.0))
        -- altHighlight
        
        >>> RB.insert (f ∷ _ "altHighlightColourDark")
            (theme.altHighlightColour # darken 0.2)
        -- fonts
        
        -- >>> RB.modify (f ∷ _ "textFontFamily") (intercalate ",")
        
        -- >>> RB.modify (f ∷ _ "headingFontFamily") (intercalate ",")
        
        -- >>> RB.modify (f ∷ _ "codeFontFamily") (intercalate ",")
        
        >>> RB.insert (f ∷ _ "isLight") isLightTheme
        -- variables
        
        >>> RB.insert (f ∷ _ "fontWeightBold") "300"
        >>> RB.insert (f ∷ _ "ratioVar") (ratio)
        >>> RB.insert (f ∷ _ "s0Var") (s0)
        >>> RB.modify (f ∷ _ "s0") (const $ reference s0)
        >>> RB.modify (f ∷ _ "ratio") (const $ reference ratio)
        >>> RB.insert (f ∷ _ "s1Var") s1
        >>> RB.insert (f ∷ _ "s2Var") s2
        >>> RB.insert (f ∷ _ "s3Var") s3
        >>> RB.insert (f ∷ _ "s4Var") s4
        >>> RB.insert (f ∷ _ "s5Var") s5
        >>> RB.insert (f ∷ _ "s_1Var") s_1
        >>> RB.insert (f ∷ _ "s_2Var") s_2
        >>> RB.insert (f ∷ _ "s_3Var") s_3
        >>> RB.insert (f ∷ _ "s_4Var") s_4
        >>> RB.insert (f ∷ _ "s_5Var") s_5
        >>> RB.insert (f ∷ _ "s1") (reference s1)
        >>> RB.insert (f ∷ _ "s2") (reference s2)
        >>> RB.insert (f ∷ _ "s3") (reference s3)
        >>> RB.insert (f ∷ _ "s4") (reference s4)
        >>> RB.insert (f ∷ _ "s5") (reference s5)
        >>> RB.insert (f ∷ _ "s_1") (reference s_1)
        >>> RB.insert (f ∷ _ "s_2") (reference s_2)
        >>> RB.insert (f ∷ _ "s_3") (reference s_3)
        >>> RB.insert (f ∷ _ "s_4") (reference s_4)
        >>> RB.insert (f ∷ _ "s_5") (reference s_5)
    )
    theme
  where
  ratio ∷ CSSVariable (Size Calc)
  ratio = variable "ratio" (unitless theme.ratio !* unitless 1.0)
  s5 = variable "s5" (reference s4 !* reference ratio)
  s4 = variable "s4" (reference s3 !* reference ratio)
  s3 = variable "s3" (reference s2 !* reference ratio)
  s2 = variable "s2" (reference s1 !* reference ratio)
  s1 = variable "s1" (reference s0 !* reference ratio)
  s0 = variable "s0" (theme.s0)
  s_1 = variable "s-1" (reference s0 !/ reference ratio)
  s_2 = variable "s-2" (reference s_1 !/ reference ratio)
  s_3 = variable "s-3" (reference s_2 !/ reference ratio)
  s_4 = variable "s-4" (reference s_3 !/ reference ratio)
  s_5 = variable "s-5" (reference s_4 !/ reference ratio)
  isLightTheme = isLight theme.backgroundColour
  lighter ∷ Color -> Color
  lighter = if isLightTheme then lighten 0.04 else lighten 0.04
  darker ∷ Color -> Color
  darker = darken 0.05
  darkerBackground ∷ Color -> Color
  darkerBackground = if isLightTheme then desaturate (0.05) <<< darken 0.03 else darken 0.05
