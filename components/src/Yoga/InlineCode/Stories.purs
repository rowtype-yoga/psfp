module Yoga.InlineCode.Stories where

import Prelude hiding (add)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import React.Basic.DOM as R
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.InlineCode.Component as InlineCode

stories ∷ Effect Storybook
stories = do
  storiesOf "InlineCode" do
    addDecorator fullScreenDecorator
    add "The InlineCode" InlineCode.makeComponent
      [ { dispatch: const $ log "hi"
        , className: Nothing
        }
      ]
