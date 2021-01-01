module Storybook.Decorator.FullScreen where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks (element)
import Yoga.Block as Block
import Yoga.Block.Container.Style (DarkOrLightMode(..))
import Yoga.Theme (fromTheme)
import Yoga.Theme.Default (darkTheme, lightTheme)
import Yoga.Theme.Types (CSSTheme)

fullScreenDecorator ∷ Effect JSX -> Effect JSX
fullScreenDecorator mkChild = do
  let
    dark = fromTheme darkTheme
    light = fromTheme lightTheme
  child <- mkChild
  pure
    $ R.div
        { style:
          css
            { minWidth: "100vw"
            , minHeight: "100vh"
            , display: "flex"
            }
        , children:
          [ element Block.container
              { themeVariant: Just DarkMode
              , content: child
              }
          ]
        }

type NamedTheme =
  { name ∷ String, theme ∷ CSSTheme }
