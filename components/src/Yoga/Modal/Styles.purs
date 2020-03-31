module Yoga.Modal.Styles where

import Prelude hiding (top)
import CSS (backgroundColor, borderRadius, boxShadow, nil, rem, toHexString)
import CSS as Color
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
    , "@keyframes zoomIn" ∷ a
    , "@keyframes fadeIn" ∷ a
    , zoomIn ∷ a
    , fadeIn ∷ a
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
        { fill: theme.textColour
        , width: "calc(0.8em * var(--ratio))"
        , height: "calc(0.8em * var(--ratio))"
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
        -- , overflow: "hidden"
        -- , lineHeight: "1.5rem"
        -- , whiteSpace: "nowrap"
        -- , height: "3rem"
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
      , "@keyframes zoomIn":
        { "from":
          { opacity: 0
          , transform: "scale3d(0.1, 0.1, 0.1) translate3d(0, 100vh, 0)"
          , "animation-timing-function": "cubic-bezier(0.55, 0.055, 0.675, 0.19)"
          }
        , "60%":
          { opacity: 1
          , transform: "scale3d(0.475, 0.475, 0.475) translate3d(0, -20vh, 0)"
          , "animation-timing-function": "cubic-bezier(0.175, 0.885, 0.32, 1)"
          }
        }
      , zoomIn:
        { animation: "$zoomIn 0.90s ease-in"
        , animationFillMode: "both"
        }
      , "@keyframes fadeIn":
        { from:
          { opacity: 0
          }
        , to:
          { opacity: 1
          }
        }
      , fadeIn:
        { animation: "$fadeIn 0.70s"
        , animationFillMode: "both"
        }
      }
