module Yoga.FillInTheGapsDraggable.Styles where

import Prelude hiding (top)
import CSS (backgroundColor, borderRadius, color, fontSize, fontStyle)
import CSS.FontStyle (italic)
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ()

type Props
  = Record PropsR

type Classes a
  = ( codeContainer ∷ a
    , solutionContainer ∷ a
    )

styles ∷ JSSClasses YogaTheme Props (Classes (JSSElem Props))
styles =
  jssClasses \theme@{ s0 } ->
    { codeContainer:
      do
        borderRadius s0 s0 s0 s0
        backgroundColor theme.backgroundColourDarker
    , solutionContainer:
      do
        fontStyle italic
        fontSize theme.s1
        color theme.highlightColour
    }
