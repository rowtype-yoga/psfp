module PSLayout where

import Prelude

import Color (toHexString)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Semigroup.Foldable (intercalateMap)
import Data.String as String
import Effect (Effect)
import JSS (jss, jssClasses)
import Justifill (justifill)
import Milkis.Impl (FetchImpl)
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM (unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactChildren, component, componentWithChildren, element, reactChildrenToArray)
import React.Basic.Hooks as React
import Yoga.CompileEditor.Component (mkCompileEditor)
import Yoga.Header.Component (mkHeader)
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
  sidebar <- mkSidebar
  header <- mkHeader
  yogaInlineCode <- InlineCode.makeComponent
  h <- mkH
  p <- mkP
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
    let
      baseline child = element cssBaseline { kids: child }

      siteInfoJSX =
        R.div
          { children:
            [ jsx header { className: "" } [ R.text siteInfo.site.siteMetadata.title ]
            , element sidebar { links: siteInfo.site.siteMetadata.menuLinks }
            , R.div_ (reactChildrenToArray children)
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
            element h { level: H4, text: props.children , className: Nothing}
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
            if isCode then
              element editor
                { initialCode: fromMaybe "" codeQ
                , height
                , language
                }
            else
              element (unsafeCreateDOMComponent "pre") props
        }
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
