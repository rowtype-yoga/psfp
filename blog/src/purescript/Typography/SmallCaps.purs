module Typography.SmallCaps where

import Prelude
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import Yoga as Y

mkSmallCaps :: Effect (String -> JSX)
mkSmallCaps = do
  React.component "SmallCaps" \text -> React.do
    pure
      $ Y.styled R.span'
          { className: "small-caps"
          , css:
              E.css
                { fontVariant: E.str "small-caps"
                }
          }
          [ R.text $ text ]
