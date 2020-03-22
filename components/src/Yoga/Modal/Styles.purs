module Yoga.Modal.Styles where

import Prelude hiding (top)
import CSS (backgroundColor, black, borderRadius, boxShadow, fromHexString, hotpink, nil, rem)
import JSS (JSSClasses, JSSElem, jssClasses)
import Yoga.Helpers ((?||))
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
          backgroundColor (fromHexString theme.interfaceColour ?|| hotpink)
          boxShadow nil nil (3.5 # rem) black
          borderRadius boxBorderRadius boxBorderRadius boxBorderRadius boxBorderRadius
      , closeIcon:
        { fill: theme.backgroundColour
        , width: "calc(0.67 * var(--s1))"
        , height: "calc(0.67 * var(--s1))"
        , margin: 0
        , padding: 0
        , marginTop: "var(--s-4)"
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
      }
