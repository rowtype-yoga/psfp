module Yoga.InlineCode.Stories where

import Prelude hiding (add)

import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Justifill (justifill)
import Milkis.Impl.Window (windowFetch)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R
import React.Basic.Extra.Hooks.UseAffReducer (useAffReducer)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, element, reactComponent)
import React.Basic.Hooks as React
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (NodeModule, Storybook, add, addDecorator, storiesOf)
import Yoga.Compiler.Api (apiCompiler)
import Yoga.InlineCode.Component as InlineCode
import Yoga.Modal.Component as Modal

stories ∷ NodeModule -> Effect Storybook
stories = do
  storiesOf "InlineCode" do
    addDecorator fullScreenDecorator
    add "The InlineCode" InlineCode.makeComponent
      [ ( justifill
            { update: log
            }
        )
      ]
    add "The InlineCode with some context" mkWrapper
      [ { inside:
          \inlineCode ->
            [ R.text "hi!"
            , element inlineCode (justifill { update: log })
            , R.text "hello again!"
            ]
        }
      ]
    add "The InlineCode with some code" mkWrapper
      [ { inside:
          \inlineCode ->
            [ R.code_ [ R.text "main = logShow \"" ]
            , element inlineCode (justifill { update: log })
            , R.code_ [ R.text "\"" ]
            ]
        }
      ]
    add "The InlineCode with some real code" mkRealWrapper
      [ {} ]

codePrefix ∷ String
codePrefix =
  """module Main where
import Batteries

main = log """"

codeSuffix ∷ String
codeSuffix = "\""

renderCode ∷ String -> Array JSX
renderCode c = intercalate [ R.br {} ] (toJSX <$> lines)
  where
  toJSX line = [ R.code_ [ R.text line ] ]
  lines = split (Pattern "\n") c

mkWrapper ∷ Effect (ReactComponent { inside ∷ ReactComponent InlineCode.Props -> Array JSX })
mkWrapper = do
  inlineCode <- InlineCode.makeComponent
  reactComponent "InlineCodeWrapper" \{ inside } -> React.do
    pure
      $ R.div_ (inside inlineCode)

type State
  = Maybe Boolean

data RealAction
  = InlineCodeSubmitted String
  | CloseModal

derive instance eqRealAction ∷ Eq RealAction
mkRealWrapper ∷ Effect (ReactComponent {})
mkRealWrapper = do
  inlineCode <- InlineCode.makeComponent
  modal <- Modal.makeComponent
  reactComponent "InlineCodeWrapper" \{} -> React.do
    state /\ dispatch <- useAffReducer Nothing realReducer
    pure
      $ R.div_
          [ R.h2_ [ R.text "Let's log some 'Magick'" ]
          , fragment $ renderCode codePrefix
          , element inlineCode (justifill { update: dispatch <<< InlineCodeSubmitted, width: String.length "Magick" })
          , fragment $ renderCode codeSuffix
          , R.br {}
          , case state of
              Just true -> jsx modal { title: "Success!" } [ R.text "You did it!" ]
              Just false -> jsx modal { title: "Oh no!" } [ R.text "Try again!" ]
              Nothing -> mempty
          ]

realReducer ∷ State -> RealAction -> Aff State
realReducer state = case _ of
  CloseModal -> pure Nothing
  InlineCodeSubmitted code -> do
    res <- (apiCompiler windowFetch).compileAndRun { code: codePrefix <> code <> codeSuffix }
    (pure <<< pure) case res of
      Right { stdout }
        | String.dropRight 1 stdout == "Magick" -> true
      _ -> false
