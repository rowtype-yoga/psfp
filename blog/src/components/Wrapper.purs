module Wrapper where

import Prelude
import Effect (Effect)
import React.Basic (JSX, ReactComponent, fragment)
import React.Basic.Hooks (ReactChildren, element, reactChildrenToArray, reactComponentWithChildren)
import Yoga.Block.Container as Container

mkWrapper ∷
  Effect
    ( ReactComponent
        { children ∷ ReactChildren JSX
        }
    )
mkWrapper =
  reactComponentWithChildren "Wrapper" \{ children } -> React.do
    pure
      $ element Container.component
          { content: fragment $ reactChildrenToArray children
          }
