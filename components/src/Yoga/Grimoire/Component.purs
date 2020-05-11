module Yoga.Grimoire.Component where

import Prelude
import Data.Array (length, mapWithIndex, zip, (!!), (..))
import Data.Array as Array
import Data.FoldableWithIndex (findWithIndex)
import Data.Interpolate as Interp
import Data.Lens (set)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Symbol (SProxy(..))
import Data.Traversable (for, sequence)
import Data.Tuple.Nested ((/\), type (/\))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class.Console (log)
import Justifill (justifill)
import Partial.Unsafe (unsafeCrashWith)
import React.Basic (ReactComponent)
import React.Basic.DOM (css)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (Ref, component, element, readRef, useLayoutEffect, useRef, useState, writeRef)
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

orDie msg =
  fromMaybe'
    (\_ -> unsafeCrashWith msg)

idxOrThrow ∷ ∀ a. Array a -> Int -> a
idxOrThrow arr i = arr !! i # orDie (Interp.i "Invalid index " i " in array")

infixl 8 idxOrThrow as !!!

swap ∷ ∀ a. Int -> Int -> Array a -> Array a
swap i j arr = swapped # orDie "Couldn't swap"
  where
  swapped = do
    valueI <- arr !! i
    valueJ <- arr !! j
    pure $ (set (ix i) valueJ <<< set (ix j) valueI) arr

springsteen init rectsRef positionsRef arg mx my down springs = do
  init
  rects <- readRef rectsRef
  positionsBefore <- readRef positionsRef
  let
    rectDragged = rects !!! (positionsBefore !!! arg)
    currentPosDragged = rectDragged # translate (mx /\ my)
    orderedRectsBefore = positionsBefore <#> (rects !!! _)
    overlapsOtherBefore = orderedRectsBefore # findWithIndex \i rect -> i /= arg && overlaps currentPosDragged rect
  case down, overlapsOtherBefore of
    false, Just { index } -> do
      let
        newPositions = swap arg index positionsBefore
      writeRef positionsRef $ spy "new posis" newPositions
    _, _ -> mempty
  -- alles gut bis hierher, glaube ich
  positions <- readRef positionsRef
  springs.set \i -> do
    let
      j = positions !!! i
      isTheDraggedOne = i == arg
      originalRect = rects !!! i
      currentRect = rects !!! j
      leftOffset = currentRect.left - originalRect.left
      topOffset = currentRect.top - originalRect.top
    case isTheDraggedOne, down, overlapsOtherBefore of
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
      false, true, Just { index, value }
        | index == i ->
          { x: rectDragged.left - currentRect.left + leftOffset
          , y: rectDragged.top - currentRect.top + topOffset
          , zIndex: 0
          , shadow: 2
          , height: "100px"
          , transform: "scale3d(1.0, 1.0, 1.0)"
          , borderRadius: "var(--s-2)"
          , immediate: const false
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
        , immediate: const true
        , transform: "scale3d(1.0,1.0,1.0)"
        }
    springs <- useSprings (Array.length props.spells) defaultSprings
    nodeRefs ∷ Ref (Array (Nullable (_))) <- useRef (props.spells $> Nullable.null)
    spells /\ updateSpells <- useState props.spells
    positionsRef <- useRef ([] ∷ _ Int)
    rectsRef <- useRef ([] ∷ _ DOMRect)
    initialisedRef <- useRef false
    windowSize <- useResize
    let
      init = do
        initialised <- readRef initialisedRef
        unless initialised do
          log "initialising"
          refs <- getRects nodeRefs
          positions <- readRef positionsRef
          writeRef positionsRef (0 .. (length props.spells - 1))
          let
            rects = refs <#> fromMaybe emptyDomRect
          writeRef rectsRef rects
          writeRef initialisedRef true
    useLayoutEffect windowSize do
      springs.set defaultSprings
      positionsBefore <- readRef positionsRef
      when (positionsBefore == []) do
        log "resetting positions"
        writeRef positionsRef (0 .. (length props.spells - 1))
      positions <- readRef positionsRef
      updateSpells (const $ positions <#> (spells !!! _))
      nodes <- readRef nodeRefs
      writeRef nodeRefs (positions <#> (nodes !!! _))
      writeRef initialisedRef false
      init
      mempty
    theme ∷ CSSTheme <- useTheme
    bindDragProps <-
      useDrag (justifill { filterTaps: true }) \{ arg, down, movement: mx /\ my } ->
        springsteen init rectsRef positionsRef arg mx my down springs
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
      $ jsx grid {} (renderSpells (spells `zip` springs.styles))

foreign import unsafeArraySetAt ∷ ∀ a. Int -> a -> Array a -> Array a

unsafeUpdateRefs ∷ Ref (Array (Nullable Node)) -> Int -> Nullable Node -> Array (Nullable Node)
unsafeUpdateRefs refsRef i el = unsafeArraySetAt i el ((unsafeCoerce refsRef).current)
