module Yoga.Centre.Styles where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( max ∷ Maybe String
    , andText ∷ Maybe Boolean
    , gutters ∷ Maybe String
    , intrinsic ∷ Maybe Boolean
    )

type Props
  = Record PropsR

styles ∷ JSSClasses YogaTheme Props ( centre ∷ JSSElem Props )
styles =
  jssClasses \theme ->
    { centre:
      jss
        { boxSizing: "content-box"
        , marginLeft: "auto"
        , marginRight: "auto"
        , maxWidth: theme.measure
        , textAlign:
          \{ andText } ->
            if (andText == Just true) then
              "center"
            else
              "left"
        , paddingLeft: _.gutters >>> fromMaybe "0"
        , paddingRight: _.gutters >>> fromMaybe "0"
        }
    }
