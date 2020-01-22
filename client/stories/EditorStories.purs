module EditorStories where

import Prelude hiding (add)

import CompileEditor.Component (mkCompileEditor)
import Decorator.FullScreen (fullScreenDecorator)
import Effect (Effect)
import Storybook.React (Storybook, add, addDecorator, storiesOf)

stories ∷ Effect Storybook
stories = do
  storiesOf "Editor" do
    addDecorator fullScreenDecorator
    add "The Editor" mkCompileEditor
      [ {}
      ]
