module MDXProvider where

import Prelude
import Data.Array (intercalate)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Framer.Motion as M
import MDX (MDX(..))
import Milkis.Impl (FetchImpl)
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM (Props_h1, Props_h2, Props_h3, Props_h4, Props_p, Props_a, unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (ReactChildren, element, memo, reactChildrenFromArray, reactChildrenToArray, reactComponent, reactComponentWithChildren, useState)
import React.Basic.Hooks as React
import Shared.Models.Body (CompileResult)
import Typography.SmallCaps (mkSmallCaps)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.CompileEditor.Component (mkCompileEditor)
import Yoga.Compiler.Api (apiCompiler)
import Yoga.Compiler.Types (Compiler)
import Yoga.FillInTheGaps.Component as FillInTheGaps
import Yoga.FillInTheGaps.Logic (Segment, findResult, parseSegments, toCode)
import Yoga.Helpers ((?||))
import Yoga as Y

type PreProps
  = { children ∷
        Nullable
          { props ∷
              Nullable
                { mdxType ∷ Nullable String
                , children ∷ Nullable String
                , className ∷ Nullable String
                }
          }
    }

isQuiz ∷ MDX -> Boolean
isQuiz (MDX a) =
  a.props.mdxType == "pre"
    && ((unsafeCoerce a).props.children.props.className == "language-purescript")
    && ((unsafeCoerce a).props.children.props.mdxType == "code")
    && ((unsafeCoerce a).props.children.props.children # parseSegments # isJust)

type MdxProviderProps
  = { children ∷ ReactChildren MDX
    }

mkLiveMdxProviderComponent ∷ FetchImpl -> Effect (ReactComponent MdxProviderProps)
mkLiveMdxProviderComponent fetchImpl = mkMdxProviderComponent (apiCompiler fetchImpl)

mkMdxProviderComponent ∷
  ∀ r.
  { | Compiler r } ->
  Effect (ReactComponent MdxProviderProps)
mkMdxProviderComponent compiler = do
  editor <- memo $ mkCompileEditor compiler
  quiz <- memo $ mkQuiz compiler
  smallCaps <- mkSmallCaps
  reactComponentWithChildren "MDXProviderComponent" do
    \{ children } -> React.do
      visibleUntil /\ updateVisible <- useState 1
      let
        onFailure title kids = log "Oopsie daisy"

        onSuccess = updateVisible (_ + one)

        mapVisible ∷ Int -> MDX -> _
        mapVisible i kid =
          { accum: i + if isQuiz kid then one else zero
          , value: if (i < visibleUntil) then Just kid else Nothing
          }

        visibleKids ∷ Array MDX
        visibleKids =
          reactChildrenToArray children
            # mapAccumL mapVisible zero
            # _.value
            # Array.catMaybes

        contentMDX ∷ ReactChildren MDX
        contentMDX = reactChildrenFromArray visibleKids

        paragraph parChildren =
          E.element M.p
            ( { layout: M.layout true
              , css:
                  E.css
                    { fontFamily: E.str "Cormorant Garamond"
                    , fontSize: E.str "min(calc(var(--s0) + 4vw), var(--s1))"
                    , "-ms-hyphens": E.str "auto"
                    , "-webkit-hyphens": E.str "auto"
                    , hyphens: E.str "auto"
                    }
              , className: "blog-p"
              , children: parChildren
              }
            )
      let
        mdxComponents =
          { "Sc":
              \(props :: { children :: String }) ->
                smallCaps props.children
          , h1:
              \(props ∷ { | M.MotionProps Props_h1 }) ->
                React.element M.h1 (props { layout = M.layout true })
          , h2:
              \(props ∷ { | M.MotionProps Props_h2 }) ->
                React.element M.h2 (props { layout = M.layout true })
          , h3:
              \(props ∷ { | M.MotionProps Props_h3 }) ->
                React.element M.h3 (props { layout = M.layout true })
          , h4:
              \(props ∷ { | M.MotionProps Props_h4 }) ->
                React.element M.h4 (props { layout = M.layout true })
          , hr: (const $ R.hr {})
          , p:
              \(props ∷ { children :: Array JSX }) ->
                paragraph props.children
          , inlineCode:
              \props -> do
                R.span { children: props.children }
          , pre:
              mkFn2 \(props ∷ PreProps) other -> do
                let
                  childrenQ = Nullable.toMaybe props.children

                  propsQ = (_.props >>> Nullable.toMaybe) =<< childrenQ

                  mdxTypeQ = (_.mdxType >>> Nullable.toMaybe) =<< propsQ

                  childrenQ2 = (_.children >>> Nullable.toMaybe) =<< propsQ

                  classNameQ = (_.className >>> Nullable.toMaybe) =<< propsQ

                  isCode = fromMaybe false (mdxTypeQ <#> eq "code")

                  codeQ = childrenQ2

                  height =
                    (fromMaybe 200 >>> show >>> (_ <> "px")) do
                      code <- codeQ
                      pure
                        ( String.split (String.Pattern "\n") code
                            # Array.length
                            # \x -> (x * 12) + 100
                        )

                  language = fromMaybe "" (classNameQ >>= String.stripPrefix (String.Pattern "language-"))

                  segmentsQ = parseSegments (codeQ ?|| "")
                case isCode, language, segmentsQ of
                  true, "purescript", Just initialSegments -> element quiz { initialSegments, onFailure, onSuccess }
                  true, _, _ ->
                    element editor
                      { initialCode: fromMaybe "" codeQ
                      , height
                      , language
                      }
                  false, _, _ -> element (unsafePerformEffect $ unsafeCreateDOMComponent "pre") props
          }
      pure
        $ element mdxProvider
            { children: contentMDX
            , components: mdxComponents
            }

type QuizProps
  = { initialSegments ∷ Array (Array Segment)
    , onFailure ∷ String -> Array JSX -> Effect Unit
    , onSuccess ∷ Effect Unit
    }

mkQuiz ∷ ∀ r. { | Compiler r } -> Effect (ReactComponent QuizProps)
mkQuiz compiler = do
  fillInTheGaps <- FillInTheGaps.makeComponent
  reactComponent "Quiz" \({ initialSegments, onFailure, onSuccess } ∷ QuizProps) -> React.do
    segments /\ updateSegments <- useState initialSegments
    solvedWith /\ updateSolvedWith <- useState Nothing
    pure
      $ element fillInTheGaps
          { segments
          , run:
              launchAff_ do
                let
                  code = toCode segments
                result <- compiler.compileAndRun { code }
                liftEffect case result of
                  Right r
                    | String.stripSuffix (String.Pattern "\n") r.stdout == (findResult $ join segments) -> do
                      updateSolvedWith (const $ String.stripSuffix (String.Pattern "\n") r.stdout)
                      onSuccess
                  Right r
                    | r.stdout /= "\n" -> onFailure "Oh shit!" [ R.text r.stdout ]
                  Right r -> onFailure "Oh shit!" [ R.text r.stderr ]
                  Left (cr ∷ CompileResult) -> onFailure "Oh shit!" [ R.text (intercalate ", " (cr.result <#> _.message)) ]
          , updateSegments:
              \update -> do
                let
                  updated = update segments
                unless (segments == updated) do updateSegments (const updated)
          , solvedWith
          }

foreign import mdxProvider ∷
  ∀ r.
  ReactComponent
    { components ∷ { | r }
    , children ∷ ReactChildren MDX
    }
