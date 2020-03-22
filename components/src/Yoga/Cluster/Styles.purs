module Yoga.Cluster.Styles where

import Prelude
import CSS (AlignItemsValue, JustifyContentValue, alignItems, display, flex, flexDirection, flexStart, flexWrap, justifyContent, row, wrap)
import CSS.Common (center)
import Data.Maybe (Maybe, fromMaybe)
import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Yoga.Helpers ((?||))
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( justify ∷ Maybe JustifyContentValue
    , align ∷ Maybe AlignItemsValue
    , space ∷ Maybe String
    )

type Props
  = Record PropsR

-- [TODO] Refactor to typed CSS
styles ∷ JSSClasses YogaTheme Props ( cluster ∷ JSSElem Props )
styles =
  jssClasses \theme ->
    { cluster:
      jss \props ->
        let
          space = props.space ?|| "1rem"
        in
          { overflow: "hidden"
          , "& > *":
            ( jss do
                display flex
                flexWrap wrap
                alignItems (fromMaybe center props.align)
                justifyContent (fromMaybe flexStart props.justify)
            )
              <> jss
                  { margin: "calc(var(" <> space <> ") / 2 * -1)"
                  }
          , "& > * > *":
            { margin: "calc(" <> space <> " / 2)"
            }
          }
    }
