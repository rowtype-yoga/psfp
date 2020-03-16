module Yoga.Card.Stories where

import Prelude hiding (add)
import Yoga.Card.Component (mkCard, mkCardContent, mkCardSubtitle, mkCardTitle)
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks (component, element)
import Storybook.React (Storybook, add, addDecorator, storiesOf)

stories ∷ Effect Storybook
stories = do
  storiesOf "Card" do
    addDecorator fullScreenDecorator
    add "Example card" mkExample
      [ { title: "An example card"
        , subtitle: "It says some more"
        , content: R.text loremIpsum
        }
      ]
  where
  mkExample = do
    card <- mkCard
    cardTitle <- mkCardTitle
    cardSubtitle <- mkCardSubtitle
    cardContent <- mkCardContent
    component "ExampleCard" \{ title, subtitle, content } -> React.do
      pure
        $ R.div
            { children:
                pure
                  $ element card
                      { kids:
                          [ element cardTitle { kids: [ R.text title ] }
                          , element cardSubtitle { kids: [ R.text subtitle ] }
                          , element cardContent { kids: [ content ] }
                          ]
                      , className: ""
                      }
            }

loremIpsum ∷ String
loremIpsum =
  """PureScript is a strongly-typed, purely-functional programming language that compiles"""
    <> """ to JavaScript. It can be used to develop web applications, server side apps, and al"""
    <> """so desktop applications with use of Electron. Its syntax is mostly comparable to tha"""
    <> """t of Haskell. In addition, it introduces row polymorphism and extensible records.[2]"""
    <> """ Also, contrary to Haskell, PureScript adheres to a strict evaluation strategy."""
