module Container.Landing where

import Prelude
import Button.Component (ButtonType(..), mkButton)
import CSS.Safer (cssSafer)
import Data.Foldable (for_)
import Data.Nullable (null)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, element, readRefMaybe, useRef)
import React.Basic.Hooks as React
import SVG.Icon (trianglelogoIcon)
import SVG.Image (mkLandingPageBackground)
import Scroll.Hook (useScrollYPosition)
import Theme.Styles (makeStyles, useTheme)
import Theme.Types (CSSTheme)
import Typography.Header (mkH)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.HTMLElement (getBoundingClientRect)
import Web.HTML.HTMLElement as HTMLElement

mkLandingPage ∷ Effect (ReactComponent {})
mkLandingPage = do
  useStyles <-
    makeStyles \(theme ∷ CSSTheme) ->
      { landing:
        cssSafer
          { width: "100vw"
          , height: "100vh"
          , gridArea: "landing"
          }
      , textLayer:
        cssSafer
          { position: "absolute"
          , width: "100%"
          , height: "100%"
          , zIndex: 2
          }
      , topBar:
        cssSafer
          { width: "100%"
          , display: "flex"
          , flexDirection: "row"
          , height: "90px"
          , justifyContent: "center"
          , alignItems: "space-around"
          }
      , logo:
        cssSafer
          { fill: theme.textColour
          , width: "50px"
          , height: "50px"
          , marginTop: "33px"
          }
      , welcomeText:
        cssSafer
          { fontSize: "36pt"
          , marginTop: "33px"
          , marginLeft: "5vw"
          }
      , welcomeCopy:
        cssSafer
          { fontSize: "22pt"
          , marginTop: "20px"
          , marginLeft: "5vw"
          , maxWidth: "400px"
          }
      , actionButton:
        cssSafer
          { marginLeft: "5vw"
          , display: "flex"
          }
      , actualActionButton:
        cssSafer
          { paddingLeft: "10px"
          , paddingRight: "10px"
          }
      , landingImage:
        cssSafer
          { position: "absolute"
          , width: "100%"
          , height: "100%"
          }
      }
  h <- mkH
  button <- mkButton
  backgroundImage <- mkLandingPageBackground
  component "LandingPage" \{} -> React.do
    classes <- useStyles
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
                        [ element button
                            { buttonProps:
                              { onClick:
                                handler_ do
                                  maybeNode <- readRefMaybe ref
                                  for_ (maybeNode >>= HTMLElement.fromNode) \n -> do
                                    height <- getBoundingClientRect n <#> _.height
                                    win <- window
                                    runEffectFn1 ((unsafeCoerce win).scrollTo)
                                      { top: height, left: 0, behavior: "smooth" }
                              }
                            , buttonType: HighlightedButton
                            , kids: [ R.text buttonText ]
                            , className: classes.actualActionButton
                            }
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
