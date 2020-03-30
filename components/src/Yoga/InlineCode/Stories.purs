module Yoga.InlineCode.Stories where

import Prelude hiding (add)
import Data.Array (intercalate, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits as String
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Justifill (justifill)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R
import React.Basic.Extra.Hooks (useAffReducer)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.CompileEditor.Component (compileAndRun)
import Yoga.InlineCode.Component (Action(..))
import Yoga.InlineCode.Component as InlineCode
import Yoga.Modal.Component as Modal

stories ∷ Effect Storybook
stories = do
  storiesOf "InlineCode" do
    addDecorator fullScreenDecorator
    add "The InlineCode" InlineCode.makeComponent
      [ ( justifill
            { dispatch: \(_ ∷ InlineCode.Action) -> ((log "hi") ∷ Effect Unit)
            }
        )
      ]
    add "The InlineCode with some context" mkWrapper
      [ { inside:
          \inlineCode ->
            [ R.text "hi!"
            , element inlineCode (justifill { dispatch: \(_ ∷ InlineCode.Action) -> (log "Hi") ∷ Effect Unit })
            , R.text "hello again!"
            ]
        }
      ]
    add "The InlineCode with some code" mkWrapper
      [ { inside:
          \inlineCode ->
            [ R.code_ [ R.text "main = logShow \"" ]
            , element inlineCode (justifill { dispatch: \(_ ∷ InlineCode.Action) -> (log "Hi") ∷ Effect Unit })
            , R.code_ [ R.text "\"" ]
            ]
        }
      ]
    add "The InlineCode with some real code" mkRealWrapper
      [ {} ]

codePrefix =
  """module Main where
import Batteries

main = log """"

codeSuffix = "\""

renderCode c = intercalate [ R.br {} ] (toJSX <$> lines)
  where
  toJSX line = [ R.code_ [ R.text line ] ]
  lines = split (Pattern "\n") c

mkWrapper ∷ Effect (ReactComponent { inside ∷ ReactComponent InlineCode.Props -> Array JSX })
mkWrapper = do
  inlineCode <- InlineCode.makeComponent
  component "InlineCodeWrapper" \{ inside } -> React.do
    pure
      $ R.div_ (inside inlineCode)

type State
  = Maybe Boolean

data RealAction
  = InlineCodeAction InlineCode.Action
  | CloseModal

derive instance eqRealAction ∷ Eq RealAction
mkRealWrapper ∷ Effect (ReactComponent {})
mkRealWrapper = do
  inlineCode <- InlineCode.makeComponent
  modal <- Modal.makeComponent
  component "InlineCodeWrapper" \{} -> React.do
    state /\ dispatch <- useAffReducer Nothing realReducer
    pure
      $ R.div_
          [ R.h2_ [ R.text "Let's log some 'Magick'" ]
          , fragment $ renderCode codePrefix
          , element inlineCode (justifill { dispatch: dispatch <<< InlineCodeAction, width: String.length "Magick" })
          , fragment $ renderCode codeSuffix
          , R.br {}
          , case state of
              Just true -> element modal $ justifill { title: "Success!", content: R.text "You did it!", onClose: dispatch CloseModal }
              Just false -> element modal $ justifill { title: "Oh no!", content: R.text "Try again!", onClose: dispatch CloseModal }
              Nothing -> mempty
          ]

realReducer ∷ State -> RealAction -> Aff State
realReducer state = case _ of
  CloseModal -> pure Nothing
  InlineCodeAction (CompileAndRunCode code) -> do
    res <- compileAndRun (M.fetch windowFetch) { code: codePrefix <> code <> codeSuffix }
    (pure <<< Just) case spy "res" res of
      Right { stdout }
        | String.dropRight 1 stdout == "Magick" -> true
      _ -> false
