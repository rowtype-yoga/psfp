module Container.Landing where

import Prelude
import Data.Foldable (for_)
import Data.Nullable (null)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import JSS (jss, jssClasses)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, component, element, readRefMaybe, useRef)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.HTMLElement (getBoundingClientRect)
import Web.HTML.HTMLElement as HTMLElement
import Yoga.Button.Component (ButtonType(..), mkButton)
import Yoga.SVG.Icon (trianglelogoIcon)
import Yoga.SVG.Image (mkLandingPageBackground)
import Yoga.Scroll.Hook (useScrollYPosition)
import Yoga.Theme.Styles (makeStylesJSS, useTheme)
import Yoga.Theme.Types (CSSTheme)
import Yoga.Typography.Header (mkH)

mkLandingPage ∷ Effect (ReactComponent {})
mkLandingPage = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { landing:
            jss
              { width: "100vw"
              , height: "100vh"
              , gridArea: "landing"
              }
          , textLayer:
            jss
              { position: "absolute"
              , width: "100%"
              , height: "100%"
              , zIndex: 2
              }
          , topBar:
            jss
              { width: "100%"
              , display: "flex"
              , flexDirection: "row"
              , height: "90px"
              , justifyContent: "center"
              , alignItems: "space-around"
              }
          , logo:
            jss
              { fill: theme.textColour
              , width: "50px"
              , height: "50px"
              , marginTop: "33px"
              }
          , welcomeText:
            jss
              { fontSize: "36pt"
              , marginTop: "33px"
              , marginLeft: "5vw"
              }
          , welcomeCopy:
            jss
              { fontSize: "22pt"
              , marginTop: "20px"
              , marginLeft: "5vw"
              , maxWidth: "400px"
              }
          , actionButton:
            jss
              { marginLeft: "5vw"
              , display: "flex"
              }
          , actualActionButton:
            jss
              { paddingLeft: "10px"
              , paddingRight: "10px"
              }
          , landingImage:
            jss
              { position: "absolute"
              , maxWidth: "none"
              , width: "100%"
              , height: "100%"
              }
          }
  h <- mkH
  button <- mkButton
  backgroundImage <- mkLandingPageBackground
  component "LandingPage" \{} -> React.do
    classes <- useStyles {}
    theme <- useTheme
    ref <- useRef null
    scrollY <- useScrollYPosition
    pure
      $ R.div
          { ref
          , className: classes.landing
          , children:
            [ element backgroundImage { className: classes.landingImage }
            , R.div
                { className: classes.textLayer
                , children:
                  [ R.div
                      { className: classes.topBar
                      , children: [ element trianglelogoIcon { className: classes.logo } ]
                      }
                  , R.div { className: classes.welcomeText, children: [ R.text welcomeText ] }
                  , R.div { className: classes.welcomeCopy, children: [ R.text copyText ] }
                  , R.div
                      { className: classes.actionButton
                      , children:
                        [ jsx button
                            { onClick:
                              handler_ do
                                maybeNode <- readRefMaybe ref
                                for_ (maybeNode >>= HTMLElement.fromNode) \n -> do
                                  height <- getBoundingClientRect n <#> _.height
                                  win <- window
                                  runEffectFn1 ((unsafeCoerce win).scrollTo)
                                    { top: height, left: 0, behavior: "smooth" }
                            , buttonType: HighlightedButton
                            , className: classes.actualActionButton
                            }
                            [ R.text buttonText ]
                        ]
                      }
                  ]
                }
            ]
          }

welcomeText ∷ String
welcomeText = "Enter the college of Kleisli"

copyText ∷ String
copyText = "Learn how to tame and harness the compiler, whether you're a λ or a Λ"

buttonText ∷ String
buttonText = "I can't wait, let's go!"
