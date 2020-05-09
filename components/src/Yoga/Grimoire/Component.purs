module Yoga.Grimoire.Component where

import Prelude
import Data.Array (mapWithIndex, zip, (!!))
import Data.Array as Array
import Data.FoldableWithIndex (findWithIndex)
import Data.Interpolate as Interp
import Data.Lens (set)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Symbol (SProxy(..))
import Data.Traversable (for, sequence)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Justifill (justifill)
import React.Basic (ReactComponent)
import React.Basic.DOM (css)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (Ref, component, element, readRef, useLayoutEffect, useRef, writeRef)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animatedDiv, useSprings)
import React.Basic.Hooks.UseGesture (useDrag, withDragProps)
import Record as Record
import Record.Extra (pick)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)
import Web.HTML.HTMLElement (DOMRect, getBoundingClientRect)
import Web.HTML.HTMLElement as HTMLElement
import Yoga.Grid.Component as Grid
import Yoga.Grimoire.Spell.Component as GrimoireSpell
import Yoga.Grimoire.Styles (styles)
import Yoga.Helpers ((?||))
import Yoga.Resize.Hook (useResize)
import Yoga.Spell.Types (Spell)
import Yoga.Theme.Styles (makeStylesJSS, useTheme)
import Yoga.Theme.Types (CSSTheme)

type Props
  = { spells ∷ Array Spell
    }

getRects ref = do
  refNodes <- readRef ref
  for
    refNodes \nullinger ->
    sequence do
      Nullable.toMaybe nullinger >>= HTMLElement.fromNode <#> getBoundingClientRect

center ∷ DOMRect -> (Number /\ Number)
center { left, top, width, height } = (left + width / 2.0) /\ (top + height / 2.0)

overlaps ∷ DOMRect -> DOMRect -> Boolean
overlaps r1 { left, top, right, bottom } = between left right x && between top bottom y
  where
  x /\ y = center r1

emptyDomRect ∷ DOMRect
emptyDomRect = { top: 0.0, right: 0.0, bottom: 0.0, left: 0.0, width: 0.0, height: 0.0 }

translate ∷ (Number /\ Number) -> DOMRect -> DOMRect
translate (x /\ y) { top, right, bottom, left, width, height } =
  { top: top + y
  , right: right + x
  , bottom: bottom + y
  , left: left + x
  , width
  , height
  }

swap ∷ ∀ a. Int -> Int -> Array a -> Maybe (Array a)
swap i j arr = do
  valueI <- arr !! i
  valueJ <- arr !! j
  pure $ (set (ix i) valueJ <<< set (ix j) valueI) arr

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  grid <- Grid.makeComponent
  spellComponent <- GrimoireSpell.makeComponent
  useStyles <- makeStylesJSS styles
  component "Grimoire" \(props ∷ Props) -> React.do
    {} <- useStyles (pick props)
    let
      defaultSprings = \_ ->
        { x: 0.0
        , y: 0.0
        , height: "100px"
        , shadow: 2
        , borderRadius: "var(--s-2)"
        , zIndex: 0
        , immediate: const false
        , transform: "scale3d(1.0,1.0,1.0)"
        }
    springs <-
      useSprings (Array.length props.spells) defaultSprings
    nodeRefs <- useRef (props.spells $> Nullable.null)
    positionsRef <- useRef ([] ∷ _ DOMRect)
    originalPositionsRef <- useRef ([] ∷ _ DOMRect)
    initialisedRef <- useRef false
    windowSize <- useResize
    let
      init = do
        initialised <- readRef initialisedRef
        unless initialised do
          rects <- getRects nodeRefs
          writeRef positionsRef (rects <#> fromMaybe emptyDomRect)
          writeRef originalPositionsRef (rects <#> fromMaybe emptyDomRect)
          writeRef initialisedRef true
    useLayoutEffect windowSize do
      springs.stop
      writeRef initialisedRef false
      springs.set defaultSprings
      mempty
    theme ∷ CSSTheme <- useTheme
    bindDragProps <-
      useDrag (justifill { filterTaps: true }) \{ arg, down, movement: mx /\ my } -> do
        init
        originalPositions <- readRef originalPositionsRef
        positionsBefore <- readRef positionsRef
        let
          originalPosDraggedBefore = positionsBefore !! arg ?|| emptyDomRect

          currentPosDraggedBefore = originalPosDraggedBefore # translate (mx /\ my)

          overlapsOtherBefore = positionsBefore # findWithIndex \i' pos -> i' /= arg && overlaps currentPosDraggedBefore pos
        case down, overlapsOtherBefore of
          false, Just { index, value } -> do
            let
              swapped = swap arg index positionsBefore ?|| positionsBefore
            writeRef positionsRef swapped
          _, _ -> mempty
        positions <- readRef positionsRef
        let
          originalPosDragged = positions !! arg ?|| emptyDomRect

          currentPosDragged = originalPosDragged # translate (mx /\ my)

          overlapsOther = positions # findWithIndex \i' pos -> i' /= arg && overlaps currentPosDragged pos
        springs.set \i -> do
          let
            iPos = positions !! i ?|| emptyDomRect

            origIPos = originalPositions !! i ?|| emptyDomRect

            leftOffset = if iPos == origIPos then 0.0 else iPos.left - origIPos.left

            topOffset = if iPos == origIPos then 0.0 else iPos.top - origIPos.top
          case i == arg, down, overlapsOther of
            false, true, Just { index, value }
              | index == i ->
                { x: originalPosDragged.left - value.left + leftOffset
                , y: originalPosDragged.top - value.top + topOffset
                , zIndex: 0
                , shadow: 2
                , height: "100px"
                , transform: "scale3d(1.0, 1.0, 1.0)"
                , borderRadius: "var(--s-2)"
                , immediate: const false
                }
            true, true, _ ->
              { x: mx + leftOffset
              , y: my + topOffset
              , zIndex: 1
              , transform: "scale3d(1.1, 1.1, 1.1)"
              , borderRadius: "var(--s-2)"
              , height: "100px"
              , shadow: 20
              , immediate: \n -> n == "x" || n == "y" || n == "zIndex"
              }
            _, _, _ ->
              { x: leftOffset
              , y: topOffset
              , shadow: 2
              , height: "100px"
              , borderRadius: "var(--s-2)"
              , zIndex: 0
              , transform: "scale3d(1.0, 1.0, 1.0)"
              , immediate: const false
              }
    let
      renderSpells =
        mapWithIndex \i (spell /\ style) ->
          animatedDiv
            $ { style: css (Record.insert (SProxy ∷ _ "boxShadow") ((unsafeCoerce (style.shadow ∷ Int)).interpolate (\s -> Interp.i "rgba(0, 0, 0, 0.15) 0px " s "px " (2 * s) "px 0px" ∷ String)) style)
              , ref: unsafeCoerce (unsafeUpdateRefs nodeRefs i)
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
