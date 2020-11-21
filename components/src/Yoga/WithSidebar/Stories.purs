module Yoga.WithSidebar.Stories where

import Prelude hiding (add)

import Data.Monoid (power)
import Effect (Effect)
import Justifill (justifill)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.WithSidebar.Component as WithSidebar

stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "WithSidebar" do
    addDecorator fullScreenDecorator
    add "The WithSidebar Component" WithSidebar.makeComponent
      [ justifill
          { sidebarChildren:
            [ R.text $ power "In the sidebar" 30
            ]
          , notSidebarChildren: [ R.text $ power "Not in the sidebar. " 40 ]
          }
      , justifill
          { notSidebarChildren: [ R.input { style: css { width: "100%" } } ]
          , sidebarChildren: [ R.button { children: [ R.text "Hit me" ] } ]
          , sidebarRight: true
          }
      ]
