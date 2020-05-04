module Yoga.Grimoire.Component where

import Prelude
import Data.Array (mapWithIndex, zip)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Effect (Effect)
import Justifill (justifill)
import React.Basic (ReactComponent)
import React.Basic.DOM (css)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (Ref, component, element)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animatedDiv, useSprings)
import React.Basic.Hooks.UseGesture (useDrag, withDragProps)
import Record.Extra (pick)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)
import Yoga.Grid.Component as Grid
import Yoga.Grimoire.Spell.Component as GrimoireSpell
import Yoga.Grimoire.Styles (styles)
import Yoga.Spell.Types (Spell)
import Yoga.Theme.Styles (makeStylesJSS, useTheme)
import Yoga.Theme.Types (CSSTheme)

type Props
  = { spells ∷ Array Spell
    }

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  grid <- Grid.makeComponent
  spellComponent <- GrimoireSpell.makeComponent
  useStyles <- makeStylesJSS styles
  component "Grimoire" \(props ∷ Props) -> React.do
    {} <- useStyles (pick props)
    springs <- useSprings (Array.length props.spells) \_ -> { x: 0.0, y: 0.0, zIndex: 0, immediate: false, transform: "scale3d(1.0,1.0,1.0)" }
    let
      _ = spy "springs" springs
    theme ∷ CSSTheme <- useTheme
    bindDragProps <-
      useDrag (justifill {}) \{ arg, down, movement: mx /\ my } -> do
        springs.set \i ->
          if i == arg && down then
            { x: mx, y: my, zIndex: 1, transform: "scale3d(1.1, 1.1, 1.1)", immediate: \n -> n == "x" || n == "y" || n == "zIndex" }
          else
            { x: 0.0, y: 0.0, zIndex: 0, transform: "scale3d(1.0, 1.0, 1.0)", immediate: const false }
    let
      renderSpells =
        mapWithIndex \i (spell /\ style) ->
          animatedDiv
            $ { style: css style
              , children:
                [ element
                    spellComponent
                    { spell
                    , cardRef: Nothing
                    }
                ]
              }
                `withDragProps`
                  bindDragProps i
    pure
      $ jsx grid {} (renderSpells (props.spells `zip` springs.styles))

foreign import unsafeArraySetAt ∷ ∀ a. Int -> a -> Array a -> Array a

unsafeUpdateRefs ∷ Ref (Array (Nullable Node)) -> Int -> Nullable Node -> Array (Nullable Node)
unsafeUpdateRefs refsRef i el = unsafeArraySetAt i el ((unsafeCoerce refsRef).current)
