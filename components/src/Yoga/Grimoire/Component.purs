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
import Data.Traversable (for, sequence)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Justifill (justifill)
import Partial.Unsafe (unsafeCrashWith)
import React.Basic (ReactComponent)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (Ref, element, reactComponent, readRef, useLayoutEffect, useRef, useState, writeRef)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (animatedDiv, useSprings)
import React.Basic.Hooks.UseGesture (useDrag, withDragProps)
import Record (disjointUnion)
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

getRects :: Ref (Array (Nullable Node))
                     -> Effect
                          (Array
                             (Maybe
                                { bottom :: Number
                                , height :: Number
                                , left :: Number
                                , right :: Number
                                , top :: Number
                                , width :: Number
                                }
                             )
                          )
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

orDie :: forall t23. String -> Maybe t23 -> t23
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

regularScale :: String
regularScale = "scale3d(1.0, 1.0, 1.0)"

scaledUp :: String
scaledUp = "scale3d(1.1, 1.1, 1.1)"

scaledCompletely :: String
scaledCompletely = "scale3d(2.0, 2.0, 2.0)"

defaultSprings :: forall t84.
  { immediate :: t84 -> Boolean
  , shadow :: Int
  , transform :: String
  , x :: Number
  , y :: Number
  , zIndex :: Int
  }
defaultSprings =
  { x: 0.0
  , y: 0.0
  , shadow: 1
  , zIndex: 0
  , immediate: const true
  , transform: "scale3d(1.0,1.0,1.0)"
  }

springsteen :: forall t111 t117 t123 t124 t125 t172.
  Discard t124 => { set :: (Int
                            -> { immediate :: String -> Boolean
                               , shadow :: Int
                               , transform :: String
                               , x :: Number
                               , y :: Number
                               , zIndex :: Int
                               }
                           )
                           -> Effect t125
                  | t172
                  }
                  -> Effect t124
                     -> t111
                        -> Ref
                             (Array
                                { bottom :: Number
                                , height :: Number
                                , left :: Number
                                , right :: Number
                                , top :: Number
                                , width :: Number
                                }
                             )
                           -> Ref (Array Int)
                              -> { arg :: Int
                                 , down :: Boolean
                                 , movement :: Number /\ Number
                                 , tap :: t123
                                 | t117
                                 }
                                 -> Effect t125
springsteen springs init windowSize rectsRef positionsRef { arg, movement: mx /\ my, down, tap } = do
  init
  rects <- readRef rectsRef
  positionsBefore <- readRef positionsRef
  let
    rectDragged = rects !!! (positionsBefore !!! arg)
    currentPosDragged = rectDragged # translate (mx /\ my)
    orderedRectsBefore = positionsBefore <#> (rects !!! _)
    overlapsOther = orderedRectsBefore # findWithIndex \i rect -> i /= arg && overlaps currentPosDragged rect
  case down, overlapsOther of
    false, Just { index } -> do
      let
        newPositions = swap arg index positionsBefore
      writeRef positionsRef newPositions
    _, _ -> mempty
  positions <- readRef positionsRef
  springs.set \i -> do
    let
      originalRect = rects !!! i
      currentRect = rects !!! (positions !!! i)
      leftOffset = currentRect.left - originalRect.left
      topOffset = currentRect.top - originalRect.top
    case down, overlapsOther of
      true, _
        | i == arg ->
          defaultSprings
            { x = mx + leftOffset
            , y = my + topOffset
            , zIndex = 1
            , transform = scaledUp
            , shadow = 10
            , immediate = \n -> n == "x" || n == "y" || n == "zIndex"
            }
      true, Just { index, value }
        | index == i ->
          defaultSprings
            { x = rectDragged.left - currentRect.left + leftOffset
            , y = rectDragged.top - currentRect.top + topOffset
            , immediate = \n -> n == "zIndex"
            }
      _, _ ->
        defaultSprings
          { x = leftOffset
          , y = topOffset
          , immediate = const false
          }

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  grid <- Grid.makeComponent
  spellComponent <- GrimoireSpell.makeComponent
  useStyles <- makeStylesJSS styles
  reactComponent "Grimoire" \(props ∷ Props) -> React.do
    classes <- useStyles (pick props)
    springs <- useSprings (Array.length props.spells) (const defaultSprings)
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
      springs.set (const defaultSprings)
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
      useDrag (justifill { filterTaps: true }) \input ->
        springsteen springs init windowSize rectsRef positionsRef input
    let
      renderSpells =
        mapWithIndex \i (spell /\ style) ->
          R.div
            { style: css { height: "100px", overflow: "visible" }
            , children:
              [ animatedDiv
                  $ { style:
                      css
                        $ disjointUnion
                            { boxShadow: ((unsafeCoerce (style.shadow ∷ Int)).to \s -> Interp.i "rgba(0, 0, 0, 0.15) 0px " s "px " (2 * s) "px 0px" ∷ String)
                            , position: "relative"
                            }
                            style
                    , className: classes.container
                    , ref: unsafeCoerce (unsafeUpdateRefs nodeRefs i)
                    , children: [ element spellComponent { spell } ]
                    }
                      `withDragProps`
                        bindDragProps i
              ]
            }
    pure
      $ jsx grid {}
      $ renderSpells (spells `zip` springs.styles)

foreign import unsafeArraySetAt ∷ ∀ a. Int -> a -> Array a -> Array a

unsafeUpdateRefs ∷ Ref (Array (Nullable Node)) -> Int -> Nullable Node -> Array (Nullable Node)
unsafeUpdateRefs refsRef i el = unsafeArraySetAt i el ((unsafeCoerce refsRef).current)
