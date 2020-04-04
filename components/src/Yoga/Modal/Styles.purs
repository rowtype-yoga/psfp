module Yoga.Modal.Styles where

import Prelude hiding (top)
import CSS (backgroundColor, borderRadius, boxShadow, nil, opacity, rem, toHexString, transform, transforms, transition)
import CSS as Color
import CSS.Transform (scaleZ)
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( 
    )

type Props
  = Record PropsR

type Classes a
  = ( box ∷ a
    , closeIcon ∷ a
    , darkOverlay ∷ a
    , dialog ∷ a
    , dialogBoxStack ∷ a
    , title ∷ a
    , titleCluster ∷ a
    )

styles ∷ JSSClasses YogaTheme Props (Classes (JSSElem Props))
styles =
  jssClasses \theme ->
    let
      boxBorderRadius = 0.33 # rem
    in
      { darkOverlay:
        { width: "100vw"
        , height: "100vh"
        , backdropFilter: "blur(6px) brightness(67%)"
        }
      , box:
        do
          backgroundColor theme.interfaceColour
          boxShadow nil nil (3.5 # rem) (Color.rgba 0 0 0 0.5)
          borderRadius boxBorderRadius boxBorderRadius boxBorderRadius boxBorderRadius
      , closeIcon:
        { stroke: theme.textColour
        , strokeWidth: "var(--s0)"
        , width: "var(--s1)"
        , height: "var(--s1)"
        , margin: 0
        , padding: 0
        }
      , titleCluster:
        { "& > * ":
          { flexWrap: "nowrap !important"
          , alignItems: "flex-start !important"
          }
        }
      , title:
        { textOverflow: "ellipsis"
        }
      , dialog:
        { width: "var(--measure) !important"
        }
      , dialogBoxStack:
        { maxWidth:
          "80.0vw !important"
        , maxHeight:
          "10.0 vh !important"
        }
      }
