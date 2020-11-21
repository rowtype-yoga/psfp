module Yoga.Imposter.Stories where

import Prelude hiding (add)
import CSS (backgroundColor, backgroundImage, height, hotpink, peachpuff, position, relative, rgba, vGradient, vh, vw, width)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Effect (Effect)
import JSS (jssClasses)
import Justifill (justifill)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.Helpers (ifJustTrue)
import Yoga.Imposter.Component as Imposter
import Yoga.Theme.Styles (makeStylesJSS)

stories ∷ _ -> Effect Storybook
stories = do
  storiesOf "Imposter" do
    addDecorator fullScreenDecorator
    add "Basic" mkExample
      [ { imposterProps:
          justifill
            { kids:
              [ R.text "I'm centred"
              ]
            }
        , higherThanView: Just false
        }
      ]
    add "Lots of text (breakout false)" mkExample
      [ { imposterProps:
          justifill
            { kids:
              [ R.text (power "I'm a lot of text. " 100)
              ]
            , breakout: false
            }
        , higherThanView: Just false
        }
      ]
    add "Lots of text (breakout true)" mkExample
      [ { imposterProps:
          justifill
            { kids:
              [ R.text (power "I'm a lot of text. " 100)
              ]
            , breakout: true
            }
        , higherThanView: Just false
        }
      ]
    add "Fixed Position" mkExample
      [ { imposterProps:
          justifill
            { kids:
              [ R.text "Here's some message."
              ]
            , fixed: true
            }
        , higherThanView: Just true
        }
      ]

type ExampleProps
  = { imposterProps ∷ Imposter.Props
    , higherThanView ∷ Maybe Boolean
    }

mkExample ∷ Effect (ReactComponent ExampleProps)
mkExample = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \_ ->
          { example:
            do
              position relative
              backgroundColor (rgba 90 90 90 0.7)
          , higherThanView:
            do
              height (200.0 # vh)
          , container:
            do
              width (100.0 # vw)
              backgroundImage $ vGradient hotpink peachpuff
          }
  imposter <- Imposter.makeComponent
  component "ImposterExample" \{ imposterProps, higherThanView } -> React.do
    pure $ R.div {}
    -- element_ props
    classes <- useStyles {}
    pure
      $ R.div
          { className: classes.container <> " " <> ifJustTrue higherThanView classes.higherThanView
          , children:
            [ element imposter
                ( imposterProps
                    { kids =
                      [ R.div
                          { className: classes.example
                          , children: imposterProps.kids
                          }
                      ]
                    }
                )
            ]
          }
