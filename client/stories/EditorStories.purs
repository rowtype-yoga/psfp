module EditorStories where

import Prelude hiding (add)

import Decorator.FullScreen (fullScreenDecorator)
import Editor (mkEditor)
import Effect (Effect)
import Storybook.React (Storybook, add, addDecorator, storiesOf)

stories ∷ Effect Storybook
stories = do
  storiesOf "Editor" do
    addDecorator fullScreenDecorator
    add "The Editor" mkEditor
      [ {}
      ]
