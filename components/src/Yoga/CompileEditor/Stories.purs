module Yoga.CompileEditor.Stories where

import Prelude hiding (add)
import Yoga.CompileEditor.Component (mkCompileEditor)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Effect (Effect)
import Milkis.Impl.Window (windowFetch)
import Storybook.React (Storybook, add, addDecorator, storiesOf)

stories ∷ Effect Storybook
stories = do
  storiesOf "Editor" do
    addDecorator fullScreenDecorator
    add "The Editor" (mkCompileEditor windowFetch)
      [ { initialCode
        , height: "30vh"
        , language: "purescript"
        }
      ]

initialCode ∷ String
initialCode =
  """module Main where

import Batteries

main :: Effect Unit
main = log "Hi, there""""
