module Landing where

import Prelude
import Data.Foldable (for_)
import Data.Nullable (null)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Icon.Logo (logo)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, reactComponent, readRefMaybe, useRef)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.HTMLElement (getBoundingClientRect)
import Web.HTML.HTMLElement as HTMLElement
import Yoga ((/>), (</*), (</*>), (</>))
import Yoga.Block as Block
import Yoga.Block.Atom.Button.Types as ButtonType
import Yoga.Block.Container.Style (colour)
import Yoga.SVG.Image (mkLandingPageBackground)
import Yoga.Scroll.Hook (useScrollYPosition)

classes =
  { landing:
    E.css
      { width: E.str "100vw"
      , height: E.str "100vh"
      , gridArea: E.str "landing"
      }
  , textLayer:
    E.css
      { position: E.str "absolute"
      , width: E.str "100%"
      , height: E.str "100%"
      , zIndex: E.int 2
      }
  , topBar:
    E.css
      { width: E.str "100%"
      , display: E.str "flex"
      , flexDirection: E.str "row"
      , height: E.str "90px"
      , justifyContent: E.str "center"
      , alignItems: E.str "space-around"
      }
  , logo:
    E.css
      { fill: E.str colour.text
      , width: E.str "50px"
      , height: E.str "50px"
      , marginTop: E.str "33px"
      }
  , welcomeText:
    E.css
      { fontSize: E.str "36pt"
      , marginTop: E.str "33px"
      , marginLeft: E.str "5vw"
      }
  , welcomeCopy:
    E.css
      { fontSize: E.str "22pt"
      , marginTop: E.str "20px"
      , marginLeft: E.str "5vw"
      , maxWidth: E.str "400px"
      }
  , actionButton:
    E.css
      { marginLeft: E.str "5vw"
      , display: E.str "flex"
      }
  , actualActionButton:
    E.css
      { paddingLeft: E.str "10px"
      , paddingRight: E.str "10px"
      }
  , landingImage:
    E.css
      { position: E.str "absolute"
      , maxWidth: E.str "none"
      , width: E.str "100%"
      , height: E.str "100%"
      }
  }

mkLandingPage ∷ Effect (ReactComponent {})
mkLandingPage = do
  backgroundImage <- mkLandingPageBackground
  reactComponent "LandingPage" \{} -> React.do
    ref <- useRef null
    scrollY <- useScrollYPosition
    pure
      $ E.element R.div'
          { ref
          , css: classes.landing
          , className: "landing-page"
          , children:
            [ E.element backgroundImage { className: "landing-image", css: classes.landingImage }
            , E.element R.div'
                { className: "text-layer"
                , css: classes.textLayer
                , children:
                  [ E.element R.div'
                      { className: "top-bar"
                      , css: classes.topBar
                      , children: [ Block.icon </*> { className: "logo", css: E.css { marginTop: E.var "--s0" }, size: E.var "--s3", icon: logo } ]
                      }
                  , E.element R.div' { className: "welcome-text", css: classes.welcomeText, children: [ R.text welcomeText ] }
                  , E.element R.div' { className: "welcome-copy", css: classes.welcomeCopy, children: [ R.text copyText ] }
                  , E.element R.div'
                      { css: classes.actionButton
                      , className: "action-button"
                      , children:
                        [ Block.button
                            </* { onClick:
                                handler_ do
                                  maybeNode <- readRefMaybe ref
                                  for_ (maybeNode >>= HTMLElement.fromNode) \n -> do
                                    height <- getBoundingClientRect n <#> _.height
                                    win <- window
                                    runEffectFn1 ((unsafeCoerce win).scrollTo)
                                      { top: height, left: 0, behavior: "smooth" }
                              , buttonType: ButtonType.Primary
                              , css: classes.actualActionButton
                              , className: "Heinzer"
                              }
                            /> [ R.text buttonText ]
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
