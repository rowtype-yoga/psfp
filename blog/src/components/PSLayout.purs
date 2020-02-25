module PSLayout where

import Prelude

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
import Theme (fromTheme)
import Theme.Default (darkTheme)
import Theme.Provider (mkThemeProvider)
import Typography.Header (HeadingLevel(..), mkH)
import Typography.Paragraph (mkP)
import Unsafe.Coerce (unsafeCoerce)

type SiteQueryResult
  = { site ∷ { siteMetadata ∷ { title ∷ String } } }

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
  cssBaseline <- mkCssBaseline
  editor <- mkCompileEditor fetchImpl
  h <- mkH
  p <- mkP
  componentWithChildren "MDXLayout" \{ children, siteInfo } -> React.do
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
            element h { level: H2, text: props.children, className: Nothing }
        , h2:
          \props ->
            element h { level: H3, text: props.children, className: Nothing }
        , h3:
          \props ->
            element h { level: H4, text: props.children, className: Nothing }
        , p:
          \props ->
            element p { text: props.children }
        , pre:
          \( props ∷
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
          ) -> do
            let
              childrenQ = Nullable.toMaybe props.children

              propsQ = (_.props >>> Nullable.toMaybe) =<< childrenQ

              mdxTypeQ = (_.mdxType >>> Nullable.toMaybe) =<< propsQ

              childrenQ2 = (_.children >>> Nullable.toMaybe) =<< propsQ

              classNameQ = (_.className >>> Nullable.toMaybe) =<< propsQ

              isCode = fromMaybe false (mdxTypeQ <#> eq "code")

              codeQ = childrenQ2

              height = (fromMaybe 200 >>> show >>> (_ <> "px")) do 
                code <- codeQ 
                pure (String.split (String.Pattern "\n") code
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
      $ element themeProvider
          { theme: fromTheme darkTheme
          , children:
            [ baseline
                [ element mdxProvider
                    { children: [ siteInfoJSX ]
                    , components: mdxComponents
                    }
                ]
            ]
          }

foreign import mdxProvider ∷
  ∀ r.
  ReactComponent
    { components ∷ { | r } -- TODO Don't lie
    , children ∷ Array JSX
    }
