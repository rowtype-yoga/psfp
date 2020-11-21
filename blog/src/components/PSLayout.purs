module PSLayout where

import Prelude
import Components.Container as Container
import Data.Array (foldMap, intercalate)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (guard)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import JSS (jssClasses)
import Justifill (justifill)
import Milkis.Impl (FetchImpl)
import React.Basic (JSX, ReactComponent, fragment)
import React.Basic.DOM (Props_h1, Props_h2, Props_h3, Props_p, css, unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactChildren, element, memo, mkReducer, reactChildrenFromArray, reactChildrenToArray, reactComponent, reactComponentWithChildren, useReducer, useState)
import React.Basic.Hooks as React
import Shared.Models.Body (CompileResult)
import Unsafe.Coerce (unsafeCoerce)
import Yoga (el)
import Yoga.Box.Component as Box
import Yoga.ClickAway.Component as ClickAway
import Yoga.CloseIcon.Component as CloseIcon
import Yoga.CompileEditor.Component (mkCompileEditor)
import Yoga.Compiler.Api (apiCompiler)
import Yoga.Compiler.Types (Compiler)
import Yoga.Cover.Component as Cover
import Yoga.FillInTheGaps.Component as FillInTheGaps
import Yoga.FillInTheGaps.Logic (Segment, findResult, parseSegments, toCode)
import Yoga.Header.Component (mkHeader)
import Yoga.Helpers ((?||))
import Yoga.Imposter.Component as Imposter
import Yoga.InlineCode.Component as InlineCode
import Yoga.Modal.Component as Modal
import Yoga.Theme (fromTheme)
import Yoga.Theme.CSSBaseline (mkCssBaseline)
import Yoga.Theme.Default (darkTheme)
import Yoga.Theme.Default as Default
import Yoga.Theme.Provider (mkThemeProvider)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

type SiteQueryResult =
  { siteMetadata ∷
    { title ∷ String
    , menuLinks ∷ Array { name ∷ String, link ∷ String }
    }
  }

type PreProps =
  { children ∷
    Nullable
      { props ∷
        Nullable
          { mdxType ∷ Nullable String
          , children ∷ Nullable String
          , className ∷ Nullable String
          }
      }
  }

data ShowModal
  = ShowModal Modal.Props
  | HideModal

mkLayout ∷
  FetchImpl ->
  Effect
    ( ReactComponent
        { children ∷ ReactChildren JSX
        , siteInfo ∷ SiteQueryResult
        }
    )
mkLayout fetchImpl = do
  themeProvider <- mkThemeProvider
  modal <- Modal.makeComponent
  imposter <- Imposter.makeComponent
  cover <- Cover.makeComponent
  clickAway <- ClickAway.makeComponent
  reducer <-
    mkReducer case _, _ of
      _, ShowModal props -> Just props
      _, HideModal -> Nothing
  mdxProviderComponent <- memo $ mkMdxProviderComponent (apiCompiler fetchImpl)
  reactComponentWithChildren "MDXLayout" \{ children, siteInfo } -> React.do
    (maybeModalProps ∷ Maybe Modal.Props) /\ dispatch <- useReducer Nothing reducer
    pure
      $ el themeProvider
          { theme: fromTheme darkTheme }
          [ el R.div'
              { style: css { maxWidth: "none" } }
              [ maybeModalProps
                  # foldMap \modalProps ->
                      fragment
                        [ element clickAway { allowEscape: Just true, onClick: dispatch HideModal, style: Nothing }
                        , element modal (justifill modalProps)
                        ]
              , element Container.component
                  { children:
                    reactChildrenFromArray
                      [ element mdxProviderComponent
                          { children
                          , siteInfo
                          , showModal: dispatch <<< ShowModal
                          , hideModal: dispatch HideModal
                          }
                      ]
                  }
              ]
          ]

isQuiz ∷ JSX -> Boolean
isQuiz a =
  (unsafeCoerce a).props.mdxType == "pre"
    && ((unsafeCoerce a).props.children.props.className == "language-purescript")
    && ((unsafeCoerce a).props.children.props.mdxType == "code")
    && ((unsafeCoerce a).props.children.props.children # parseSegments # isJust)

type MdxProviderProps =
  { children ∷ ReactChildren JSX
  , siteInfo ∷ SiteQueryResult
  , showModal ∷ Modal.Props -> Effect Unit
  , hideModal ∷ Effect Unit
  }

mkMdxProviderComponent ∷ ∀ r. { | Compiler r } -> Effect (ReactComponent MdxProviderProps)
mkMdxProviderComponent compiler = do
  cssBaseline <- memo (mkCssBaseline Default.fontFaces)
  editor <- memo $ mkCompileEditor compiler
  box <- memo $ Box.makeComponent
  header <- memo mkHeader
  yogaInlineCode <- InlineCode.makeComponent
  quiz <- memo $ mkQuiz compiler
  closeIcon <- CloseIcon.makeComponent
  reactComponentWithChildren "MDXProviderComponent" \({ children, siteInfo, showModal, hideModal } ∷ MdxProviderProps) -> React.do
    visibleUntil /\ updateVisible <- useState 1
    let
      onFailure title kids =
        showModal
          ( justifill
              { title
              , icon: element closeIcon { onClick: hideModal, style: Nothing }
              , kids
              } ∷
              Modal.Props
          )
      onSuccess = updateVisible (_ + one)
      mapVisible i kid =
        { accum: i + if isQuiz kid then one else zero
        , value: guard (i < visibleUntil) (pure kid)
        }
      visibleKids ∷ Array JSX
      visibleKids =
        reactChildrenToArray children
          # mapAccumL mapVisible zero
          # _.value
          # Array.catMaybes
      siteInfoJSX =
        R.div
          { children:
            [ jsx header { className: "" } [ R.text siteInfo.siteMetadata.title ]
            , jsx box {} visibleKids
            ]
          }
    mdxComponents /\ _ <-
      useState
        { h1:
          \(props ∷ { | Props_h1 }) -> R.h1 props
        , h2:
          \(props ∷ { | Props_h2 }) -> R.h2 props
        , h3:
          \(props ∷ { | Props_h3 }) -> R.h3 props
        , hr: (const $ R.hr {})
        , p:
          \(props ∷ { | Props_p }) -> R.p props
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
          { children: pure $ jsx cssBaseline {} [ siteInfoJSX ]
          , components: mdxComponents
          }

type QuizProps =
  { initialSegments ∷ Array (Array Segment)
  , onFailure ∷ String -> Array JSX -> Effect Unit
  , onSuccess ∷ Effect Unit
  }

mkQuiz ∷ ∀ r. { | Compiler r } -> Effect (ReactComponent QuizProps)
mkQuiz compiler = do
  fillInTheGaps <- FillInTheGaps.makeComponent
  box <- Box.makeComponent
  reactComponent "Quiz" \({ initialSegments, onFailure, onSuccess } ∷ QuizProps) -> React.do
    segments /\ updateSegments <- useState initialSegments
    solvedWith /\ updateSolvedWith <- useState Nothing
    pure
      $ jsx box {}
          [ element fillInTheGaps
              { segments
              , incantate:
                launchAff_ do
                  let code = toCode segments
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
                  let updated = update segments
                  case segments, updated of
                    old, new
                      | new == old -> mempty
                    _, _ -> updateSegments (const updated)
              , solvedWith
              }
          ]

foreign import mdxProvider ∷
  ∀ r.
  ReactComponent
    { components ∷ { | r } -- TODO Don't lie
    , children ∷ Array JSX
    }
