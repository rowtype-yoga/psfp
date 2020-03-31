module Yoga.Layer.Stories where

import Prelude hiding (add)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Justifill (justifill)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R
import React.Basic.Extra.Hooks.UseAffReducer (useAffReducer)
import React.Basic.Hooks (ReactComponent, component, element)
import React.Basic.Hooks as React
import Storybook.Decorator.FullScreen (fullScreenDecorator)
import Storybook.React (Storybook, add, addDecorator, storiesOf)
import Yoga.CompileEditor.Component (compileAndRun)
import Yoga.Layer.Component (Action(..))
import Yoga.Layer.Component as Layer
import Yoga.Modal.Component as Modal

stories ∷ Effect Storybook
stories = do
  storiesOf "Layer" do
    addDecorator fullScreenDecorator
    add "The Layer" Layer.makeComponent
      [ ( justifill
            { dispatch: \(_ ∷ Layer.Action) -> ((log "hi") ∷ Effect Unit)
            }
        )
      ]
    add "The Layer with some context" mkWrapper
      [ { inside:
          \inlineCode ->
            [ R.text "hi!"
            , element inlineCode (justifill { dispatch: \(_ ∷ Layer.Action) -> (log "Hi") ∷ Effect Unit })
            , R.text "hello again!"
            ]
        }
      ]
    add "The Layer with some code" mkWrapper
      [ { inside:
          \inlineCode ->
            [ R.code_ [ R.text "main = logShow \"" ]
            , element inlineCode (justifill { dispatch: \(_ ∷ Layer.Action) -> (log "Hi") ∷ Effect Unit })
            , R.code_ [ R.text "\"" ]
            ]
        }
      ]
    add "The Layer with some real code" mkRealWrapper
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

mkWrapper ∷ Effect (ReactComponent { inside ∷ ReactComponent Layer.Props -> Array JSX })
mkWrapper = do
  inlineCode <- Layer.makeComponent
  component "LayerWrapper" \{ inside } -> React.do
    pure
      $ R.div_ (inside inlineCode)

type State
  = Maybe Boolean

data RealAction
  = LayerAction Layer.Action
  | CloseModal

derive instance eqRealAction ∷ Eq RealAction
mkRealWrapper ∷ Effect (ReactComponent {})
mkRealWrapper = do
  inlineCode <- Layer.makeComponent
  modal <- Modal.makeComponent
  component "LayerWrapper" \{} -> React.do
    state /\ dispatch <- useAffReducer Nothing realReducer
    pure
      $ R.div_
          [ R.h2_ [ R.text "Let's log some 'Magick'" ]
          , fragment $ renderCode codePrefix
          , element inlineCode (justifill { dispatch: dispatch <<< LayerAction, width: String.length "Magick" })
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
  LayerAction (CompileAndRunCode code) -> do
    res <- compileAndRun (M.fetch windowFetch) { code: codePrefix <> code <> codeSuffix }
    (pure <<< pure) case res of
      Right { stdout }
        | String.dropRight 1 stdout == "Magick" -> true
      _ -> false
