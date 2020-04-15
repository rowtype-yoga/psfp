module PSLayout where

import Prelude

import Data.Array (foldMap)
import Data.Array as A
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import JSS (jss, jssClasses)
import Justifill (justifill)
import Milkis.Impl (FetchImpl)
import React.Basic (JSX, ReactComponent, fragment)
import React.Basic.DOM (unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactChildren, component, componentWithChildren, element, reactChildrenToArray, useEffect, useState)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Box.Component as Box
import Yoga.ClickAway.Component as ClickAway
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
  mdxProviderComponent <- mkMdxProviderComponent (apiCompiler fetchImpl)
  componentWithChildren "MDXLayout" \{ children, siteInfo } -> React.do
    pure
      $ element themeProvider
          { theme: fromTheme darkTheme
          , children:
            [ element mdxProviderComponent
                { children
                , siteInfo
                }
            ]
          }

mkSecret ∷ Effect (ReactComponent { kids ∷ Array JSX, visible ∷ Boolean, register ∷ Effect Unit })
mkSecret = do
  component "Secret" \{ kids, visible, register } -> React.do
    useEffect visible do
      unless visible register
      pure mempty
    pure
      $ R.div
          { style: R.css { visibility: if visible then "visible" else "hidden" }
          , children: kids
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
        }
    )
mkMdxProviderComponent compiler = do
  cssBaseline <- mkCssBaseline
  editor <- mkCompileEditor compiler
  fillInTheGaps <- FillInTheGaps.makeComponent
  box <- Box.makeComponent
  sidebar <- mkSidebar
  header <- mkHeader
  yogaInlineCode <- InlineCode.makeComponent
  modal <- Modal.makeComponent
  clickAway <- ClickAway.makeComponent
  h <- mkH
  p <- mkP
  secret <- mkSecret
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
          , flexer:
            { display: "flex"
            , flexDirection: "row"
            }
          }
  componentWithChildren "MDXProviderComponent" \{ children, siteInfo } -> React.do
    classes <- useStyles {}
    visibleThrough /\ updateVisible <- useState 1
    questions /\ updateQuestions <- useState []
    maybeModal /\ updateModal <- useState Nothing
    let
      baseline child = element cssBaseline { kids: child }

      kids = reactChildrenToArray children

      foldVisible ∷ { i ∷ Int, acc ∷ Array JSX } -> JSX -> { i ∷ Int, acc ∷ Array JSX }
      foldVisible { i, acc } a =
        if i >= visibleThrough then
          { i, acc }
        else
          if isQuiz a then
            { i: i + 1, acc: A.snoc acc a }
          else
            { i, acc: A.snoc acc a }

      visibleKids = kids # A.foldl foldVisible { i: 0, acc: [] ∷ Array JSX } # _.acc

      siteInfoJSX =
        R.div
          { children:
            [ jsx header { className: "" } [ R.text siteInfo.site.siteMetadata.title ]
            , element sidebar { links: siteInfo.site.siteMetadata.menuLinks }
            , jsx box {} visibleKids
            ]
          }

      mdxComponents =
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
          \(props ∷ PreProps) -> do
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
              true, "purescript", Just initialSegments ->
                jsx box {}
                  [ element fillInTheGaps
                      { initialSegments
                      , incantate: \segments -> launchAff_ do 
                          let code = toCode segments
                          result <- compiler.compileAndRun { code }
                          liftEffect case result of
                            Right r | String.stripSuffix (String.Pattern "\n") r.stdout == (findResult $ join segments) ->
                              updateVisible (_ + 1)
                            _ ->
                              updateModal (const $ Just { title: "Failed", kids: [R.text "Try again"] })
                      }
                  ]
              true, _, _ ->
                element editor
                  { initialCode: fromMaybe "" codeQ
                  , height
                  , language
                  }
              false, _, _ -> element (unsafeCreateDOMComponent "pre") props
        }
    pure
      $ baseline
          [ element mdxProvider
              { children:
                [ siteInfoJSX
                ]
              , components: mdxComponents
              }
          , maybeModal # foldMap 
            (\modalProps -> 
            fragment [
              element clickAway { allowEscape: Just true, onClick: updateModal (const Nothing), style: Nothing },
              element modal (justifill modalProps)
            ]
            )
          ]

mkSidebar = do
  withSidebar <- WithSidebar.makeComponent
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { flexer:
            jss
              { display: "flex"
              , flexDirection: "row"
              , width: "300px"
              , height: "300px"
              }
          }
  component "Sidebar" \{ links } -> React.do
    classes <- useStyles {}
    pure
      $ element withSidebar
          ( justifill
              { sidebarChildren: [ R.div_ [ R.text "hahahaha" ] ]
              , notSidebarChildren: [] ∷ Array JSX
              }
          )

foreign import mdxProvider ∷
  ∀ r.
  ReactComponent
    { components ∷ { | r } -- TODO Don't lie
    , children ∷ Array JSX
    }
