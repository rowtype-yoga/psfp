module Yoga.SVG.Icon where

import Prelude
import Data.Monoid (guard)
import Effect (Effect)
import JSS (jss, jssClasses)
import Prim.Row (class Union)
import React.Basic.DOM.SVG as SVG
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Theme.Styles (makeStylesJSS)
import Yoga.Theme.Types (CSSTheme)

type ImageProps
  = ( width ∷ Int, height ∷ Int, className ∷ String )

type Raw r
  = ReactComponent { | r }

foreign import alternativeIconRaw ∷ ∀ r. Raw r

alternativeIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
alternativeIcon = unsafeCoerce alternativeIconRaw

foreign import apIconRaw ∷ ∀ r. Raw r

apIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
apIcon = unsafeCoerce apIconRaw

foreign import appendIconRaw ∷ ∀ r. Raw r

appendIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
appendIcon = unsafeCoerce appendIconRaw

foreign import applyIconRaw ∷ ∀ r. Raw r

applyIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
applyIcon = unsafeCoerce applyIconRaw

foreign import applyflippedIconRaw ∷ ∀ r. Raw r

applyflippedIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
applyflippedIcon = unsafeCoerce applyflippedIconRaw

foreign import bindIconRaw ∷ ∀ r. Raw r

bindIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
bindIcon = unsafeCoerce bindIconRaw

foreign import composeIconRaw ∷ ∀ r. Raw r

composeIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
composeIcon = unsafeCoerce composeIconRaw

foreign import forallIconRaw ∷ ∀ r. Raw r

forallIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
forallIcon = unsafeCoerce forallIconRaw

foreign import kleisliIconRaw ∷ ∀ r. Raw r

kleisliIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
kleisliIcon = unsafeCoerce kleisliIconRaw

foreign import mapIconRaw ∷ ∀ r. Raw r

mapIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
mapIcon = unsafeCoerce mapIconRaw

foreign import mapflippedIconRaw ∷ ∀ r. Raw r

mapflippedIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
mapflippedIcon = unsafeCoerce mapflippedIconRaw

foreign import pslogoIconRaw ∷ ∀ r. Raw r

pslogoIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
pslogoIcon = unsafeCoerce pslogoIconRaw

foreign import trianglelogoIconRaw ∷ ∀ r. Raw r

trianglelogoIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
trianglelogoIcon = unsafeCoerce trianglelogoIconRaw

foreign import playIconRaw ∷ ∀ r. Raw r

playIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
playIcon = unsafeCoerce playIconRaw

data ActiveArrowDirection
  = ArrowPointsRight
  | ArrowPointsLeft

derive instance eqActiveArrowDirection ∷ Eq ActiveArrowDirection
mkMenu ∷ Effect (ReactComponent { activeArrowDirection ∷ ActiveArrowDirection })
mkMenu = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { arrow:
            jss
              { fill: theme.textColour
              , transition: "0.3s ease-out"
              }
          , arrowInactive:
            jss
              { fillOpacity: ".1"
              }
          , svg:
            jss
              { width: "100%"
              , height: "100%"
              , "&:hover":
                { fill: theme.backgroundColour
                }
              }
          }
  component "MenuIcon" \{ activeArrowDirection } -> React.do
    classes <- useStyles {}
    pure
      $ SVG.svg
          { xmlns: "http://www.w3.org/2000/svg"
          , viewBox: "0 0 100 100"
          , fillRule: "nonzero"
          , strokeLinejoin: "round"
          , strokeMiterlimit: "2"
          , className: classes.svg
          , children:
            [ SVG.path
                { d: "M28.72 46.524l8.94.26-7.843 7.388h40.97l-7.87 6.732H29.753l7.64 7.302-8.76.267-10.928-10.88 11.013-11.07zM17.493 57.806l.213-.213-.213.213z"
                , className:
                  classes.arrow <> " "
                    <> guard (activeArrowDirection == ArrowPointsRight) classes.arrowInactive
                }
            , SVG.path
                { d: "M71.3 53.472l-8.94-.26 7.843-7.388H29.4l7.723-6.732h33.163l-7.64-7.302 8.758-.267 10.93 10.88L71.3 53.472zM82.536 42.2l-.213.213.213-.213z"
                , className:
                  classes.arrow <> " "
                    <> guard (activeArrowDirection == ArrowPointsLeft) classes.arrowInactive
                }
            ]
          }
