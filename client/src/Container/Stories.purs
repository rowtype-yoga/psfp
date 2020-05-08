module Container.Stories where

import Prelude hiding (add)
import Container.Component (mkContainer)
import Effect (Effect)
import React.Basic.Hooks (component, element)
import React.Basic.Hooks as React
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Theme.Styles (useTheme)

stories ∷ Effect Storybook
stories = do
  storiesOf "Container" do
    addDecorator fullScreenDecorator
    add "Container" mkExample [ {} ]
  where
  mkExample = do
    container <- mkContainer
    component "ContainerExample" \{} -> React.do
      theme <- useTheme
      pure
        $ element container
            { theme
            , kids: []
            }
