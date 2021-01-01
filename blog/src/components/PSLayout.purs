module PSLayout where

import Prelude
import Color as Color
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
import Effect.Unsafe (unsafePerformEffect)
import MDX (MDX(..))
import Milkis.Impl (FetchImpl)
import React.Basic (JSX, ReactComponent, fragment)
import React.Basic.DOM (Props_h1, Props_h2, Props_h3, Props_p, unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Emotion as Emotion
import React.Basic.Hooks (ReactChildren, element, memo, mkReducer, reactChildrenFromArray, reactChildrenToArray, reactComponent, reactComponentWithChildren, useReducer, useState, useState')
import React.Basic.Hooks as React
import Shared.Models.Body (CompileResult)
import Unsafe.Coerce (unsafeCoerce)
import Yoga ((/>), (</), (</*))
import Yoga as Y
import Yoga.Block as Block
import Yoga.Block.Atom.Toggle.Types (TogglePosition(..))
import Yoga.Block.Container as Container
import Yoga.Block.Container.Style (DarkOrLightMode(..), colour)
import Yoga.Block.Icon.SVG as Icon
import Yoga.Block.Internal (_0)
import Yoga.Block.Modal as Modal
import Yoga.CompileEditor.Component (mkCompileEditor)
import Yoga.Compiler.Api (apiCompiler)
import Yoga.Compiler.Types (Compiler)
import Yoga.FillInTheGaps.Component as FillInTheGaps
import Yoga.FillInTheGaps.Logic (Segment, findResult, parseSegments, toCode)
import Yoga.Helpers ((?||))

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
        { children ∷ ReactChildren MDX
        , siteInfo ∷ SiteQueryResult
        , mdxProviderComponent ∷ ReactComponent MdxProviderProps
        }
    )
mkLayout fetchImpl = do
  reducer <-
    mkReducer case _, _ of
      _, ShowModal props -> Just props
      _, HideModal -> Nothing
  -- mdxProviderComponent <- mkMdxProviderComponent (apiCompiler fetchImpl)
  reactComponentWithChildren "MDXLayout" \{ children, siteInfo, mdxProviderComponent } -> React.do
    (maybeModalProps ∷ Maybe Modal.Props) /\ dispatch <- useReducer Nothing reducer
    darkOrLightMode /\ setDarkOrLightMode <- useState' Nothing
    let
      maybeTogglePosition =
        darkOrLightMode
          <#> case _ of
              DarkMode -> ToggleIsRight
              LightMode -> ToggleIsLeft
      darkThemeToggle =
        Block.toggle
          Y.</> do
              { className: "dark-light-toggle"
              , css:
                Emotion.css
                  { marginRight: Emotion.str "0"
                  , marginLeft: Emotion.str "auto"
                  }
              , setTogglePosition:
                case _ of
                  ToggleIsLeft -> setDarkOrLightMode (Just LightMode)
                  ToggleIsRight -> setDarkOrLightMode (Just DarkMode)
              , togglePosition: fromMaybe ToggleIsLeft maybeTogglePosition
              , left:
                Block.icon
                  Y.</> do
                      { icon: Icon.moon
                      , stroke: Emotion.str "#eef"
                      }
              , right:
                Block.icon
                  Y.</> do
                      { icon: Icon.sun
                      , stroke: Emotion.str "yellow"
                      }
              , backgroundLeft:
                Color.hsl 205.0 1.0 0.80
              , backgroundRight:
                Color.hsl 250.0 1.0 0.1
              }
      header =
        Block.box
          </ { className: "top-box"
            , css:
              Emotion.css
                { background: Emotion.str colour.backgroundLayer5
                }
            }
          /> [ Block.cluster
                </* { justify: "space-between"
                  , className: "header"
                  , space: "var(--s-2)"
                  , css: Emotion.css { width: Emotion.percent 100.0 }
                  }
                /> do
                    [ Y.styled
                        R.span'
                        { className: "blog-header"
                        , css:
                          Emotion.css
                            { fontSize: Emotion.str "min(var(--s3), calc(var(--s1) + 1vw))"
                            , textTransform: Emotion.str "uppercase"
                            , letterSpacing: Emotion.str "calc(min(var(--s0) + 3vw), var(--s2) * -1.7)"
                            , fontWeight: Emotion.str "300"
                            , color: Emotion.str colour.text
                            }
                        }
                        [ R.text siteInfo.siteMetadata.title ]
                    , darkThemeToggle
                    ]
            ]
    pure
      $ element Container.component
          { themeVariant: darkOrLightMode
          , content:
            Y.styled Block.stack
              { className: "blog-content"
              , space: _0
              , splitAfter: 2
              , css:
                Emotion.css
                  { minHeight: Emotion.vh 100.0
                  , background: Emotion.str colour.background
                  , borderRadius: Emotion.str "0 var(--s1) var(--s1) 0"
                  }
              }
              [ header
              , Block.centre
                  </* { className: "centre"
                    , padding: _0
                    , gutters: _0
                    , css:
                      Emotion.css
                        { maxWidth: Emotion.ch 90.0
                        }
                    }
                  /> [ Block.box
                        </ do
                            { padding: Emotion.px 16
                            , css: Emotion.css { background: Emotion.str colour.background }
                            }
                        /> [ element mdxProviderComponent
                              { children
                              , siteInfo
                              , showModal: dispatch <<< ShowModal
                              , hideModal: dispatch HideModal
                              , modalOpen: isJust maybeModalProps
                              }
                          ]
                    ]
              , Y.styled Block.box
                  { className: "blog-footer"
                  , css:
                    Emotion.css
                      { background: Emotion.str colour.backgroundLayer1
                      , borderTop: Emotion.str colour.backgroundLayer3
                      , color: Emotion.str colour.text
                      }
                  }
                  [ R.text "Mark Eibes" ]
              ]
          }

isQuiz ∷ MDX -> Boolean
isQuiz (MDX a) =
  a.props.mdxType == "pre"
    && ((unsafeCoerce a).props.children.props.className == "language-purescript")
    && ((unsafeCoerce a).props.children.props.mdxType == "code")
    && ((unsafeCoerce a).props.children.props.children # parseSegments # isJust)

type MdxProviderProps =
  { children ∷ ReactChildren MDX
  , siteInfo ∷ SiteQueryResult
  , showModal ∷ Modal.Props -> Effect Unit
  , hideModal ∷ Effect Unit
  , modalOpen ∷ Boolean
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
  reactComponentWithChildren "MDXProviderComponent" do
    \{ children, siteInfo, showModal, hideModal, modalOpen } -> React.do
      visibleUntil /\ updateVisible <- useState 1
      let
        onFailure title kids =
          showModal
            { content: fragment kids
            , isOpen: modalOpen
            , setIsOpen: if _ then pure unit else hideModal
            }
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
      let
        mdxComponents =
          { h1:
            \(props ∷ { | Props_h1 }) -> R.h2 props
          , h2:
            \(props ∷ { | Props_h2 }) -> R.h3 props
          , h3:
            \(props ∷ { | Props_h3 }) -> R.h4 props
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
            { children: contentMDX
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
  reactComponent "Quiz" \({ initialSegments, onFailure, onSuccess } ∷ QuizProps) -> React.do
    segments /\ updateSegments <- useState initialSegments
    solvedWith /\ updateSolvedWith <- useState Nothing
    pure
      $ element fillInTheGaps
          { segments
          , run:
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
              unless (segments == updated) do updateSegments (const updated)
          , solvedWith
          }

foreign import mdxProvider ∷
  ∀ r.
  ReactComponent
    { components ∷ { | r } -- TODO Don't lie
    , children ∷ ReactChildren MDX
    }
