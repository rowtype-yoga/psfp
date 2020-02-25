module EditorStories where

import Prelude hiding (add)
import CompileEditor.Component (mkCompileEditor)
import Decorator.FullScreen (fullScreenDecorator)
import Effect (Effect)
import Milkis.Impl.Window (windowFetch)
import Storybook.React (Storybook, add, addDecorator, storiesOf)

initialCode :: String
initialCode =
  """module Main where

import Batteries

main :: Effect Unit
main = log "Hi, there""""

stories ∷ Effect Storybook
stories = do
  storiesOf "Editor" do
    addDecorator fullScreenDecorator
    add "The Editor" (mkCompileEditor windowFetch)
      [ { initialCode
        , height: "300px"
        , language: "purescript"
        }
      ]
