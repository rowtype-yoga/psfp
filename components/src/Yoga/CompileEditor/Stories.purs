module Yoga.CompileEditor.Stories where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.CompileEditor.Component (mkCompileEditor)
import Yoga.Compiler.Types (Compiler)

compiler ∷ { | Compiler () }
compiler = { compileAndRun }
  where
  compileAndRun { code } = pure (pure { code: Nothing, stderr: "", stdout: "" })

stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "Editor" do
    addDecorator fullScreenDecorator
    add "The Editor" (mkCompileEditor compiler)
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
