module Yoga.Switcher.Styles where

import Prelude
import Data.Interpolate (i)
import Data.Maybe (Maybe, fromMaybe)
import Data.String as String
import Foreign.Object as Object
import JSS (JSSClasses, JSSElem, jss, jssClasses)
import Yoga.Theme.Types (YogaTheme)

type PropsR
  = ( threshold ∷ Maybe String
    , space ∷ Maybe String
    , limit ∷ Int
    )

type Props
  = Record PropsR

or ∷ ∀ t13. Maybe t13 -> t13 -> t13
or = flip fromMaybe

styles ∷ JSSClasses YogaTheme Props ( switcher ∷ JSSElem Props )
styles =
  jssClasses \theme ->
    { switcher:
      jss \props ->
        let
          { space, threshold, limit } =
            { space: spaceFromProps props
            , threshold: props.threshold `or` theme.measure
            , limit: props.limit + 1
            }
        in
          jss
            { display: "block"
            , "& > *":
              { display: "flex"
              , flexWrap: "wrap"
              , overflow: "hidden"
              , margin: "calc((" <> space <> " / 2) * -1)"
              }
            , "& > * > *":
              { flexGrow: 1
              , flexBasis: i "calc((" threshold " - (100% - " space ")) * 999)" ∷ String
              , margin: i "calc(" space "/ 2)" ∷ String
              }
            }
            <> jss
                ( Object.singleton (i "& > * > :nth-last-child(n+" limit "), & > * > :nth-last-child(n+" limit ") ~ *")
                    { flexBasis: "100%"
                    }
                )
    }

spaceFromProps ∷ ∀ r. { space ∷ Maybe String | r } -> String
spaceFromProps { space } =
  fromMaybe "var(--s1)" do
    supplied <- space
    pure $ if String.trim supplied == "0" then "0px" else supplied
