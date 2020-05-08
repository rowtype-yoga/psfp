module PSLayout where

import Prelude

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
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import JSS (jssClasses)
import Justifill (justifill)
import Milkis.Impl (FetchImpl)
import React.Basic (JSX, ReactComponent, fragment)
import React.Basic.DOM (unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactChildren, component, componentWithChildren, element, memo, reactChildrenToArray, useReducer, useState)
import React.Basic.Hooks as React
import Shared.Models.Body (CompileResult)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Box.Component as Box
import Yoga.ClickAway.Component as ClickAway
import Yoga.CloseIcon.Component as CloseIcon
import Yoga.CompileEditor.Component (mkCompileEditor)
import Yoga.Compiler.Api (apiCompiler)
import Yoga.Compiler.Types (Compiler)
import Yoga.FillInTheGaps.Component as FillInTheGaps
import Yoga.FillInTheGaps.Logic (findResult, parseSegments, toCode)
import Yoga.Header.Component (mkHeader)
import Yoga.Helpers ((?||))
import Yoga.InlineCode.Component as InlineCode
import Yoga.Modal.Component as Modal
import Yoga.Theme (fromTheme)
import Yoga.Theme.CSSBaseline (mkCssBaseline)
import Yoga.Theme.Default (darkTheme)
import Yoga.Theme.Provider (mkThemeProvider)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)
import Yoga.Typography.Header (HeadingLevel(..), mkH)
import Yoga.Typography.Paragraph (mkP)
import Yoga.WithSidebar.Component (makeComponent) as WithSidebar

type SiteQueryResult
  = { site ∷
      { siteMetadata ∷
        { title ∷ String
        , menuLinks ∷ Array { name ∷ String, link ∷ String }
        }
      }
    }

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

data ShowModal
  = ShowModal Modal.Props
  | HideModal

mkLayout ∷
  ∀ r.
  FetchImpl ->
  Effect
    ( ReactComponent
        { children ∷ ReactChildren JSX
        , siteInfo ∷ SiteQueryResult
        }
    )
mkLayout fetchImpl = do
  themeProvider <- mkThemeProvider
  modal <- memo Modal.makeComponent
  clickAway <- memo ClickAway.makeComponent
  mdxProviderComponent <- memo $ mkMdxProviderComponent (apiCompiler fetchImpl)
  componentWithChildren "MDXLayout" \{ children, siteInfo } -> React.do
    maybeModalProps /\ dispatch <-
      useReducer Nothing case _, _ of
        _, ShowModal props -> Just props
        _, HideModal -> Nothing
    pure
      $ element themeProvider
          { theme: fromTheme darkTheme
          , children:
            [ R.div_
                [ maybeModalProps
                    # foldMap \modalProps ->
                        fragment
                          [ element clickAway { allowEscape: Just true, onClick: dispatch HideModal, style: Nothing }
                          , element modal (justifill modalProps)
                          ]
                , element mdxProviderComponent
                    { children
                    , siteInfo
                    , showModal: dispatch <<< ShowModal
                    , hideModal: dispatch HideModal
                    }
                ]
            ]
          }

isQuiz ∷ JSX -> Boolean
isQuiz a =
  (unsafeCoerce a).props.mdxType == "pre"
    && ((unsafeCoerce a).props.children.props.className == "language-purescript")
    && ((unsafeCoerce a).props.children.props.mdxType == "code")
    && ((unsafeCoerce a).props.children.props.children # parseSegments # isJust)

mkMdxProviderComponent ∷
  ∀ r.
  { | Compiler r } ->
  Effect
    ( ReactComponent
        { children ∷ ReactChildren JSX
        , siteInfo ∷ SiteQueryResult
        , showModal ∷ Modal.Props -> Effect Unit
        , hideModal :: Effect Unit
        }
    )
mkMdxProviderComponent compiler = do
  cssBaseline <- memo mkCssBaseline
  editor <- memo $ mkCompileEditor compiler
  box <- memo $ Box.makeComponent
  sidebar <- memo mkSidebar
  header <- memo mkHeader
  yogaInlineCode <- InlineCode.makeComponent
  quiz <- memo $ mkQuiz compiler
  h <- memo mkH
  p <- memo mkP
  closeIcon <- CloseIcon.makeComponent
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { code:
            { fontFamily: theme.codeFontFamily # NEA.head
            , backgroundColor: theme.interfaceColour
            , fontSize: "10pt"
            , border: "1px solid #383c39"
            , padding: "3px"
            , borderRadius: "3px"
            }
          }
  componentWithChildren "MDXProviderComponent" \{ children, siteInfo, showModal, hideModal  } -> React.do
    classes <- useStyles {}
    visibleUntil /\ updateVisible <- useState 1
    let
      onFailure title kids =
        showModal
          ( justifill
              { title
              , icon: element closeIcon { onClick: hideModal, style: Nothing }
              , kids
              } ∷ Modal.Props
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
            [ jsx header { className: "" } [ R.text siteInfo.site.siteMetadata.title ]
            , element sidebar { links: siteInfo.site.siteMetadata.menuLinks }
            , jsx box {} visibleKids
            ]
          }
    mdxComponents /\ _ <-
      useState
        { h1:
          \props ->
            element h ({ level: H2, text: props.children, className: Nothing })
        , h2:
          \props ->
            element h { level: H3, text: props.children, className: Nothing }
        , h3:
          \props ->
            element h { level: H4, text: props.children, className: Nothing }
        , hr: (const $ R.hr {})
        , p:
          \props ->
            R.div
              { children: [ element p { text: props.children } ]
              }
        , inlineCode:
          \props -> do
            R.span { className: classes.code, children: props.children }
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
              false, _, _ -> element (unsafeCreateDOMComponent "pre") props
        }
    pure
      $ element mdxProvider
          { children: pure $ jsx cssBaseline {} [ siteInfoJSX ]
          , components: mdxComponents
          }

mkQuiz ∷ _ -> Effect (ReactComponent _)
mkQuiz compiler = do
  fillInTheGaps <- FillInTheGaps.makeComponent
  box <- Box.makeComponent
  component "Quiz" \({ initialSegments, onFailure, onSuccess } ∷ { initialSegments ∷ _, onFailure ∷ _, onSuccess ∷ _ }) -> React.do
    segments /\ updateSegments <- useState initialSegments
    solvedWith /\ updateSolvedWith <- useState Nothing
    pure
      $ jsx box {}
          [ element fillInTheGaps
              { segments
              , incantate:
                launchAff_ do
                  let
                    code = toCode segments
                  result <- compiler.compileAndRun { code }
                  liftEffect case result of
                    Right r
                      | String.stripSuffix (String.Pattern "\n") r.stdout == (findResult $ join segments) -> do
                        updateSolvedWith (const $ String.stripSuffix (String.Pattern "\n") r.stdout)
                        onSuccess
                    Right r | r.stdout /= "\n" -> onFailure "Oh shit!" [R.text r.stdout]
                    Right r -> onFailure "Oh shit!" [R.text r.stderr]
                    Left (cr :: CompileResult) -> onFailure "Oh shit!" [R.text (intercalate ", " (cr.result <#> _.message))]
              , updateSegments:
                \update -> do
                  let
                    updated = update segments
                  case segments, updated of
                    old, new
                      | new == old -> mempty
                    _, _ -> updateSegments (const updated)
              , solvedWith
              }
          ]

mkSidebar = do
  withSidebar <- WithSidebar.makeComponent
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          {}
  component "Sidebar" \{ links } -> React.do
    classes <- useStyles {}
    pure
      $ element withSidebar
          ( justifill
              { sidebarChildren: [ R.div_ [] ]
              , notSidebarChildren: [] ∷ Array JSX
              }
          )

foreign import mdxProvider ∷
  ∀ r.
  ReactComponent
    { components ∷ { | r } -- TODO Don't lie
    , children ∷ Array JSX
    }
