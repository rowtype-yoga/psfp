module PSLayout where

import Prelude

import Data.Array as A
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Effect (Effect)
import JSS (jss, jssClasses)
import Justifill (justifill)
import Milkis.Impl (FetchImpl)
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM (unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactChildren, component, componentWithChildren, element, reactChildrenToArray, useEffect, useState)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Box.Component as Box
import Yoga.CompileEditor.Component (mkCompileEditor)
import Yoga.Compiler.Api (apiCompiler)
import Yoga.FillInTheGaps.Component as FillInTheGaps
import Yoga.Header.Component (mkHeader)
import Yoga.Helpers ((?||))
import Yoga.InlineCode.Component as InlineCode
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
  mdxProviderComponent <- mkMdxProviderComponent fetchImpl
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

mkSecret :: Effect ( ReactComponent { kids ∷ Array JSX , visible ∷ Boolean, register :: Effect Unit })
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

mkMdxProviderComponent ∷
  FetchImpl ->
  Effect
    ( ReactComponent
        { children ∷ ReactChildren JSX
        , siteInfo ∷ SiteQueryResult
        }
    )
mkMdxProviderComponent fetchImpl = do
  cssBaseline <- mkCssBaseline
  editor <- mkCompileEditor fetchImpl
  fillInTheGaps <- FillInTheGaps.makeComponent (apiCompiler fetchImpl)
  box <- Box.makeComponent
  sidebar <- mkSidebar
  header <- mkHeader
  yogaInlineCode <- InlineCode.makeComponent
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
    visibleThroughKey /\ updateVisible <- useState ""
    let
      baseline child = element cssBaseline { kids: child }
      kids = reactChildrenToArray children
      visibleKids = spy "visibleKids" $ fromMaybe kids do
         i <- kids # A.findIndex (\x -> (unsafeCoerce x).key == visibleThroughKey)
         pure $ A.take (i+1) kids 

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
        , p:
          \props ->
            R.div
              { children: [ element p { text: props.children } ]
              }
        -- , thematicBreak:
        --   \props -> jsx secret { register: updateSections (Map.insert )  }
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
            case isCode, language of
              true, "puregaps" -> element fillInTheGaps { code: fromMaybe "" codeQ }
              true, _ ->
                element editor
                  { initialCode: fromMaybe "" codeQ
                  , height
                  , language
                  }
              false, _ -> element (unsafeCreateDOMComponent "pre") props
        }

    useEffect (A.length kids) do
      let 
        firstGaps = kids # A.find (\x -> (unsafeCoerce x).props.mdxType == "pre" && ((unsafeCoerce x).props.children.props.className == "language-puregaps")) <#> (\x -> (unsafeCoerce x).key)
        lastKey = kids # A.last # unsafeCoerce # _.key
      updateVisible (const (firstGaps ?|| lastKey))
      pure mempty
    pure
      $ baseline
          [ element mdxProvider
              { children:
                [ siteInfoJSX
                ]
              , components: mdxComponents
              }
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
