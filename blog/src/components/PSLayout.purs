module PSLayout where

import Prelude
import Color as Color
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import MDX (MDX)
import MDXProvider (MdxProviderProps)
import Milkis.Impl (FetchImpl)
import React.Basic (ReactComponent)
import React.Basic.DOM as R
import React.Basic.Emotion as Emotion
import React.Basic.Hooks (ReactChildren, element, reactComponentWithChildren, useState')
import React.Basic.Hooks as React
import Yoga ((/>), (</), (</*))
import Yoga as Y
import Yoga.Block as Block
import Yoga.Block.Atom.Toggle.Types (TogglePosition(..))
import Yoga.Block.Container as Container
import Yoga.Block.Container.Style (DarkOrLightMode(..), colour)
import Yoga.Block.Icon.SVG as Icon
import Yoga.Block.Internal (_0)
import Yoga.Block.Modal as Modal

type SiteQueryResult =
  { siteMetadata ∷
    { title ∷ String
    , menuLinks ∷ Array { name ∷ String, link ∷ String }
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
  -- mdxProviderComponent <- mkMdxProviderComponent (apiCompiler fetchImpl)
  reactComponentWithChildren "MDXLayout" \{ children, siteInfo, mdxProviderComponent } -> React.do
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
                    , css: Emotion.css { maxWidth: Emotion.ch 90.0 }
                    }
                  /> [ Block.box
                        </ do
                            { padding: Emotion.px 16
                            , css:
                              Emotion.css
                                { "& > p:first-of-type:first-letter":
                                  Emotion.nested
                                    $ Emotion.css
                                        { overflow: Emotion.str "visible"
                                        , float: Emotion.str "left"
                                        , fontSize: Emotion.str "min(calc(var(--s0) * 1 + 8.03vw), var(--s5))"
                                        , fontFamily: Emotion.str "Cormorant Garamond"
                                        , lineHeight: Emotion.str "min(calc(var(--s-1) + 2px + 3vw), var(--s3))"
                                        , marginTop: Emotion.str "min(calc(var(--s-1) + 1.5vw), var(--s2))"
                                        , marginRight: Emotion.var "--s-2"
                                        , "&::selection":
                                          Emotion.nested
                                            $ Emotion.css
                                                { color: Emotion.str colour.highlightText
                                                , background: Emotion.str colour.highlight
                                                }
                                        }
                                }
                            }
                        /> [ element mdxProviderComponent
                              { children
                              }
                          ]
                    ]
              , Y.styled Block.box
                  { className: "blog-footer"
                  , css:
                    Emotion.css
                      { background: Emotion.str colour.backgroundLayer1
                      , borderTop: Emotion.str $ "1px solid " <> colour.backgroundLayer3
                      , color: Emotion.str colour.text
                      }
                  }
                  [ R.text "Mark Eibes" ]
              ]
          }
