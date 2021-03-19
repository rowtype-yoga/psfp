module Landing where

import Prelude
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Nullable (null)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Icon.Logo (logo)
import React.Basic (Ref)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (ReactComponent, reactComponent, readRefMaybe, useRef)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.HTMLElement (getBoundingClientRect)
import Web.HTML.HTMLElement as HTMLElement
import Yoga ((/>), (</), (</*), (</*>), (</>))
import Yoga.Block as Block
import Yoga.Block.Atom.Button.Types as ButtonType
import Yoga.Block.Container.Style (DarkOrLightMode(..), colour)
import Yoga.Block.Internal (NodeRef)
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
      , width: E.str "min(100%, 1200px)"
      , height: E.str "100%"
      }
  }

mkLandingPage ∷ Effect (ReactComponent {})
mkLandingPage = do
  backgroundImage <- mkLandingPageBackground
  reactComponent "LandingPage" \{} -> React.do
    ref <- useRef null
    scrollY <- useScrollYPosition
    let
      themeVariant ∷ DarkOrLightMode
      themeVariant = DarkMode -- [TODO]
      backgroundImg =
        E.element
          backgroundImage
          { className: "landing-image"
          , css: classes.landingImage
          , themeVariant
          }
      button =
        R.div'
          </*> { css: classes.actionButton
            , className: "action-button"
            , children:
              [ Block.button
                  </* { onClick: scrollTo ref
                    , buttonType: ButtonType.Primary
                    , css: classes.actualActionButton
                    , className: "lets-go-btn"
                    }
                  /> [ R.text buttonText ]
              ]
            }
      welcomeTitle =
        R.div'
          </*> { className: "welcome-text", css: classes.welcomeText, children: [ R.text welcomeText ] }
      welcomeSubtitle =
        R.div'
          </*> { className: "welcome-copy", css: classes.welcomeCopy, children: [ R.text copyText ] }
      upperBlock =
        E.element R.div'
          { ref
          , css: classes.landing
          , className: "landing-page"
          , children:
            [ backgroundImg
            , E.element R.div'
                { className: "text-layer"
                , css: classes.textLayer
                , children:
                  [ R.div'
                      </*> { className: "top-bar"
                        , css: classes.topBar
                        , children: [ Block.icon </*> { className: "logo", css: E.css { marginTop: E.var "--s0" }, size: E.var "--s3", icon: logo } ]
                        }
                  , Block.stack
                      </ {}
                      /> [ welcomeTitle
                        , welcomeSubtitle
                        , button
                        ]
                  ]
                }
            ]
          }
      textBlock = R.text placeholderText
      content = Block.stack </ {}
    pure $ Block.container </> { themeVariant: Just themeVariant, content: content [ upperBlock, textBlock ] }

welcomeText ∷ String
welcomeText = "Enter the college of Kleisli"

copyText ∷ String
copyText = "Let's go!"

buttonText ∷ String
buttonText = "I can't wait, let's go!"

placeholderText ∷ String
placeholderText = "Franz jagt im komplett verwahrlosten Taxi quer durch Bayern. " `power` 20

scrollTo ∷ NodeRef -> EventHandler
scrollTo ref =
  handler_ do
    maybeNode <- readRefMaybe ref
    for_ (maybeNode >>= HTMLElement.fromNode) \n -> do
      height <- getBoundingClientRect n <#> _.height
      win <- window
      runEffectFn1 ((unsafeCoerce win).scrollTo)
        { top: height, left: 0, behavior: "smooth" }
