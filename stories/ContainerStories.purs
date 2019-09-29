module ContainerStories where

import Prelude hiding (add)
import Container.Component (mkContainer)
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import Storybook.React (Storybook, add, storiesOf)
import Theme.Default (darkTheme)

stories ∷ Effect Storybook
stories =
  storiesOf "Container" do
    add "Container" mkContainer
      [ { theme: darkTheme
        , children:
          [ R.h1
              { children: [ R.text "PURESCRIPT" ]
              , style:
                css
                  { textAlign: "center"
                  , marginTop: "20px"
                  , fontSize: "2em"
                  , fontFamily: darkTheme.headingFontFamily
                  , letterSpacing: "0.07em"
                  }
              }
          , R.div
              { children: [ R.text "what the fuck?" ]
              , style: css { height: "400px", textAlign: "center", marginTop: "100px" }
              }
          ]
        }
      ]
