module ButtonStories where

import Prelude hiding (add)

import Button.Component (ButtonType(..), mkButton)
import Decorator.FullScreen (fullScreenDecorator)
import Effect (Effect)
import Effect.Console (log)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, element)
import SVG.Icon (alternativeIcon, apIcon, appendIcon, applyIcon, applyflippedIcon, bindIcon, composeIcon, forallIcon, kleisliIcon, mapIcon, mapflippedIcon, pslogoIcon)
import Storybook.React (Storybook, add, addDecorator, storiesOf)

stories ∷ Effect Storybook
stories =
  storiesOf "Button" do
    addDecorator fullScreenDecorator
    add "Button" mkExample
      [ { text: "Cancel"
        , buttonType: PlainButton
        , buttonProps: { onClick: handler_ (log "clicked cancel") }
        }
      , { text: "OK"
        , buttonType: HighlightedButton
        , buttonProps:
          { onClick: handler_ (log "clicked OK")
          }
        }
      , { text: "Disabled"
        , buttonType: DisabledButton
        , buttonProps:
          { onClick: handler_ (log "clicked Disabled")
          }
        }
      , { text: "Very long button text"
        , buttonType: PlainButton
        , buttonProps: { onClick: handler_ (log "clicked very long...") }
        }
      ]
    add "Icon Button" mkIconButton
      [ { icon: alternativeIcon, buttonType: PlainButton }
      , { icon: apIcon, buttonType: HighlightedButton }
      , { icon: appendIcon, buttonType: PlainButton }
      , { icon: applyflippedIcon, buttonType: PlainButton }
      , { icon: applyIcon, buttonType: PlainButton }
      , { icon: bindIcon, buttonType: PlainButton }
      , { icon: composeIcon, buttonType: PlainButton }
      , { icon: forallIcon, buttonType: PlainButton }
      , { icon: kleisliIcon, buttonType: DisabledButton }
      , { icon: mapflippedIcon, buttonType: PlainButton }
      , { icon: mapIcon, buttonType: PlainButton }
      , { icon: pslogoIcon, buttonType: PlainButton }
      ]
  where
  mkExample = do
    button <- mkButton
    component "ExampleButton" \{ text, buttonType, buttonProps } -> React.do
      pure
        $ element button
            { children: [ R.text text ], buttonProps, buttonType
            }

  mkIconButton = do
    button <- mkButton
    component "ExampleIconButton" \{ icon, buttonType } -> React.do
      pure
        $ element button
            { children: [ element icon { width: 30 } ]
            , buttonProps: { }
            , buttonType
            }

loremIpsum ∷ String
loremIpsum =
  """PureScript is a strongly-typed, purely-functional programming language that compiles"""
    <> """ to JavaScript. It can be used to develop web applications, server side apps, and al"""
    <> """so desktop applications with use of Electron. Its syntax is mostly comparable to tha"""
    <> """t of Haskell. In addition, it introduces row polymorphism and extensible records.[2]"""
    <> """ Also, contrary to Haskell, PureScript adheres to a strict evaluation strategy."""
