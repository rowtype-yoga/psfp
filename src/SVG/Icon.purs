module SVG.Icon where

import Prelude

import Data.Monoid (guard)
import Effect (Effect)
import Prim.Row (class Union)
import React.Basic.DOM (css)
import React.Basic.DOM.SVG as SVG
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React
import Theme.Styles (makeStyles)
import Theme.Types (Theme)
import Unsafe.Coerce (unsafeCoerce)

type ImageProps = ( width ∷ Int, height ∷ Int )

type Raw = ReactComponent {  width ∷ Int, height ∷ Int, className ∷ String }

foreign import alternativeIconRaw ∷ Raw
alternativeIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
alternativeIcon = unsafeCoerce alternativeIconRaw

foreign import apIconRaw ∷ Raw
apIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
apIcon = unsafeCoerce apIconRaw

foreign import appendIconRaw ∷ Raw
appendIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
appendIcon = unsafeCoerce appendIconRaw

foreign import applyIconRaw ∷ Raw
applyIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
applyIcon = unsafeCoerce applyIconRaw

foreign import applyflippedIconRaw ∷ Raw
applyflippedIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
applyflippedIcon = unsafeCoerce applyflippedIconRaw

foreign import bindIconRaw ∷ Raw
bindIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
bindIcon = unsafeCoerce bindIconRaw

foreign import composeIconRaw ∷ Raw
composeIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
composeIcon = unsafeCoerce composeIconRaw

foreign import forallIconRaw ∷ Raw
forallIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
forallIcon = unsafeCoerce forallIconRaw

foreign import kleisliIconRaw ∷ Raw
kleisliIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
kleisliIcon = unsafeCoerce kleisliIconRaw

foreign import mapIconRaw ∷ Raw
mapIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
mapIcon = unsafeCoerce mapIconRaw

foreign import mapflippedIconRaw ∷ Raw
mapflippedIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
mapflippedIcon = unsafeCoerce mapflippedIconRaw

foreign import pslogoIconRaw ∷ Raw
pslogoIcon ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
pslogoIcon = unsafeCoerce pslogoIconRaw



data ActiveArrowDirection
  = ArrowPointsRight
  | ArrowPointsLeft

derive instance eqActiveArrowDirection ∷ Eq ActiveArrowDirection

mkMenu ∷ Effect (ReactComponent { activeArrowDirection ∷ ActiveArrowDirection })
mkMenu = do
  useStyles <-
    makeStyles \(theme ∷ Theme) ->
      { arrow:
        css
          { fill: theme.textColour
          , transition: "0.3s ease-out"
          }
      , arrowInactive:
        css
          { fillOpacity: ".1"
          }
      , svg:
        css
          { width: "100%"
          , height: "100%"
          , "&:hover":
            css
              { fill: theme.backgroundColour
              }
          }
      }
  component "MenuIcon" \{ activeArrowDirection } -> React.do
    classes <- useStyles
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
