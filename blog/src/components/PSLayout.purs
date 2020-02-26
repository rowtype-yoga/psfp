module PSLayout where

import Prelude
import CSS.Safer (cssSafer)
import CSSBaseline (mkCssBaseline)
import CompileEditor.Component (mkCompileEditor)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Effect (Effect)
import Milkis.Impl (FetchImpl)
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM (unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactChildren, componentWithChildren, element)
import React.Basic.Hooks as React
import Theme (fromTheme)
import Theme.Default (darkTheme)
import Theme.Provider (mkThemeProvider)
import Theme.Styles (makeStyles)
import Theme.Types (CSSTheme)
import Typography.Header (HeadingLevel(..), mkH)
import Typography.Paragraph (mkP)
import Unsafe.Coerce (unsafeCoerce)

type SiteQueryResult
  = { site ∷ { siteMetadata ∷ { title ∷ String } } }

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
  ∀ children.
  FetchImpl ->
  Effect
    ( ReactComponent
        { children ∷ ReactChildren children
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

mkMdxProviderComponent ::
  ∀ children.
  FetchImpl ->
  Effect
    ( ReactComponent
        { children ∷ ReactChildren children
        , siteInfo ∷ SiteQueryResult
        }
    )
mkMdxProviderComponent fetchImpl = do
  cssBaseline <- mkCssBaseline
  editor <- mkCompileEditor fetchImpl
  h <- mkH
  p <- mkP
  useStyles <-
    makeStyles \(theme :: CSSTheme) ->
      { code:
        cssSafer
          { fontFamily: "PragmataPro Liga"
          , backgroundColor: theme.interfaceColour
          , fontSize: "10pt"
          , border: "1px solid #383c39"
          , padding: "3px"
          , borderRadius: "3px"
          }
      , margins:
        cssSafer
          { marginLeft: "35px"
          , marginRight: "35px"
          }
      }
  componentWithChildren "MDXProviderComponent" \{ children, siteInfo } -> React.do
    classes <- useStyles
    let
      baseline child = element cssBaseline { kids: child }

      siteInfoJSX =
        R.div
          { children:
            [ element h
                { level: H1
                , text: siteInfo.site.siteMetadata.title
                , className: Nothing
                }
            , R.div_ (unsafeCoerce children)
            ]
          }

      mdxComponents =
        { h1:
          \props ->
            element h { level: H2, text: props.children, className: Just classes.margins }
        , h2:
          \props ->
            element h { level: H3, text: props.children, className: Just classes.margins }
        , h3:
          \props ->
            element h { level: H4, text: props.children, className: Just classes.margins }
        , p:
          \props ->
            R.div
              { children: [ element p { text: props.children } ]
              , className: classes.margins
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
              { children: [ siteInfoJSX ]
              , components: mdxComponents
              }
          ]

foreign import mdxProvider ∷
  ∀ r.
  ReactComponent
    { components ∷ { | r } -- TODO Don't lie
    , children ∷ Array JSX
    }
