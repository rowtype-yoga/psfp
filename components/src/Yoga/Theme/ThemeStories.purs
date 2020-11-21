module Yoga.Theme.Stories where

import Prelude hiding (add)
import Color (toHexString)
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks (component, element)
import Storybook.React (Storybook, add, storiesOf)
import Yoga.Theme (fromTheme)
import Yoga.Theme.Default (darkTheme, lightTheme)

stories ∷ _ -> Effect Storybook
stories = do
  storiesOf "Theme" do
    add "The swatches in the themes" mkExample
      [ { theme: darkTheme, themeName: "Dark theme" }
      , { theme: lightTheme, themeName: "Light theme" }
      ]
  where
  mkExample = do
    swatch <- mkSwatch
    component "ExampleSwatches" \{ theme, themeName } -> React.do
      let
        cssTheme = fromTheme theme
        sw name colour = element swatch { name, colour, fontFamily: cssTheme.textFontFamily }
      pure
        $ R.div
            { style:
              css
                { color: toHexString cssTheme.textColour
                , backgroundColor: toHexString cssTheme.backgroundColour
                }
            , children:
              [ R.div
                  { style:
                    css
                      { fontSize: "2em"
                      , fontFamily: cssTheme.headingFontFamily
                      , padding: "20px 0 30px 10px"
                      }
                  , children: [ R.text themeName ]
                  }
              , R.div
                  { style:
                    css
                      { display: "grid"
                      , gridTemplateColumns: "120px 120px 120px 120px 120px 120px 120px 120px"
                      , gridRowGap: "20px"
                      , paddingBottom: "20px"
                      }
                  , children:
                    [ sw "IF Darkest" (toHexString cssTheme.interfaceColourDarkest)
                    , sw "IF Darker" (toHexString cssTheme.interfaceColourDarker)
                    , sw "Interface" (toHexString cssTheme.interfaceColour)
                    , sw "IF Lighter" (toHexString cssTheme.interfaceColourLighter)
                    , sw "IF Lightest" (toHexString cssTheme.interfaceColourLightest)
                    , sw "Highlight" (toHexString cssTheme.highlightColour)
                    , sw "Highlight++" (toHexString cssTheme.highlightColourRotatedForwards)
                    , sw "HighlightGradient" $ "linear-gradient(45deg," <> toHexString cssTheme.highlightColourRotatedForwards <> "," <> toHexString cssTheme.highlightColourRotatedBackwards <> ")"
                    , sw "TXT Darkest" (toHexString cssTheme.textColourDarkest)
                    , sw "TXT Darker" (toHexString cssTheme.textColourDarker)
                    , sw "TXT" (toHexString cssTheme.textColour)
                    , sw "TXT Lighter" (toHexString cssTheme.textColourLighter)
                    , sw "TXT Lightest" (toHexString cssTheme.textColourLightest)
                    , sw "HighlightDK" (toHexString cssTheme.highlightColourDark)
                    , sw "Highlight--" (toHexString cssTheme.highlightColourRotatedBackwards)
                    ]
                  }
              ]
            }
  mkSwatch = do
    component "Swatch" \{ name, colour, fontFamily } -> React.do
      pure
        $ R.div
            { style: css { display: "flex", flexDirection: "column", alignItems: "center" }
            , children:
              [ R.div
                  { children: [ R.text name ]
                  , style: css { fontFamily, paddingBottom: "5px" }
                  }
              , R.div
                  { style:
                    css
                      { background: colour
                      , width: "100px"
                      , height: "100px"
                      , outline: "2px solid black"
                      }
                  }
              ]
            }

loremIpsum ∷ String
loremIpsum =
  """PureScript is a strongly-typed, purely-functional programming language that compiles"""
    <> """ to JavaScript. It can be used to develop web applications, server side apps, and al"""
    <> """so desktop applications with use of Electron. Its syntax is mostly comparable to tha"""
    <> """t of Haskell. In addition, it introduces row polymorphism and extensible records.[2]"""
    <> """ Also, contrary to Haskell, PureScript adheres to a strict evaluation strategy."""
