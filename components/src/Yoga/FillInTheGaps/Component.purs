module Yoga.FillInTheGaps.Component where

import Prelude
import Data.Array (foldMap, foldr, intercalate)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function.Uncurried (mkFn2)
import Data.Lens ((%~))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe', isJust, isNothing)
import Data.Monoid (guard)
import Data.String (Pattern(..), split)
import Data.String as S
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Justifill (justifill)
import Partial.Unsafe (unsafeCrashWith)
import React.Basic (JSX, ReactComponent, element, fragment)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Helpers (jsx)
import React.Basic.Hooks (component, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Spring (useTransition)
import React.Basic.SyntaxHighlighter.Component (HighlighterTheme, syntaxHighlighterImpl)
import Shared.Models.Body (CompileResult, RunResult)
import Yoga.Button.Component (ButtonType(..), mkButton)
import Yoga.ClickAway.Component as ClickAway
import Yoga.CloseIcon.Component as CloseIcon
import Yoga.Cluster.Component as Cluster
import Yoga.Compiler.Types (Compiler)
import Yoga.Helpers ((?||))
import Yoga.InlineCode.Component as InlineCode
import Yoga.Modal.Component as Modal
import Yoga.Theme.Styles (useTheme)
import Yoga.Theme.Syntax (mkHighlighterTheme)

data Segment
  = ExpectedResult String
  | Start
  | End
  | Filler String
  | Hole Int String

derive instance eqSegment ∷ Eq Segment
getResult ∷ Segment -> Maybe String
getResult = case _ of
  ExpectedResult r -> Just r
  _ -> Nothing

findResult ∷ Array Segment -> String
findResult = fromMaybe' (\_ -> unsafeCrashWith "Even teachers make mistakes") <<< A.findMap getResult

toCode ∷ Array (Array Segment) -> String
toCode lines = intercalate "\n" mapped
  where
  mapped = lines <#> (intercalate "" <<< map segmentToCode)
  segmentToCode = case _ of
    Filler s -> s
    Hole _ s -> s
    _ -> ""

complete ∷ Array (Array Segment) -> Boolean
complete arr = foldl f true (join arr)
  where
  f acc seg =
    acc
      && case seg of
          Hole _ s -> s /= ""
          _ -> true

visibleRange ∷ Array (Array Segment) -> { end ∷ Int, start ∷ Int }
visibleRange arr = { start, end }
  where
  start = A.findIndex (_ == [ Start ]) arr ?|| 0
  end = A.findIndex (_ == [ End ]) arr ?|| A.length arr

renderSegments ∷ HighlighterTheme -> ReactComponent InlineCode.Props -> ((Array (Array Segment) -> Array (Array Segment)) -> Effect Unit) -> Array (Array Segment) -> JSX
renderSegments highlighterTheme ic update arrs = R.div_ (A.mapWithIndex renderLine arrs)
  where
  { start, end } = visibleRange arrs
  renderLine i l = R.div_ (A.mapWithIndex (renderSegment i) l)
  renderSegment i j s =
    if between start end i then case s of
      Filler s' -> element syntaxHighlighterImpl { style: highlighterTheme, language: "purescript", children: spy "ch" s' }
      Hole width _ ->
        element ic
          $ justifill
              { width
              , onSubmit: update <<< updateSegments i j
              }
      _ -> mempty
    else
      mempty

updateSegments ∷ Int -> Int -> String -> Array (Array Segment) -> Array (Array Segment)
updateSegments i j v = (ix i <<< ix j) %~ f
  where
  f = case _ of
    Hole h _ -> Hole h v
    _ -> unsafeCrashWith "Updated a non-hole"

holeRegex ∷ Regex
holeRegex = unsafeRegex "({-.*?-})" RegexFlags.global

resultRegex ∷ Regex
resultRegex = unsafeRegex "--result" RegexFlags.global

startRegex ∷ Regex
startRegex = unsafeRegex "--start here" RegexFlags.global

endRegex ∷ Regex
endRegex = unsafeRegex "--end here" RegexFlags.global

rawSegments ∷ String -> Array String
rawSegments = Regex.split holeRegex

toSegment ∷ String -> Segment
toSegment = case _ of
  x
    | Regex.test holeRegex x -> Hole (S.length x - 4) ""
  x
    | S.indexOf (Pattern "--result ") x == Just 0 -> ExpectedResult ((S.stripPrefix (Pattern "--result ") x) ?|| "")
  x
    | Regex.test startRegex x -> Start
  x
    | Regex.test endRegex x -> End
  other -> Filler other

-- Glues together Filler segments
-- [[Filler "line1"], [Filler "line2"]] --> [[Filler "line1\nline2"]]
-- [[Filler "line1", Filler "line2"]] --> [[Filler "line1\nline2"]]
-- [[Filler "line1", Filler "line2"], [Filler "line3"]] --> [[Filler "line1\nline2\line3"]]
smooshFillers ∷ Array (Array Segment) -> Array (Array Segment)
smooshFillers = foldl smooshOuter []
  where
  smooshInner segments segment = case segment, A.unsnoc segments of
    Filler f, Just { init, last: Filler prev } -> A.snoc init (Filler (prev <> "\n" <> f))
    _, _ -> A.snoc segments segment
  smooshOuter acc segs = case foldl smooshInner [] segs, A.unsnoc acc of
    [ Filler f ], Just { init, last: [ Filler prev ] } -> A.snoc init [ Filler (prev <> "\n" <> f) ]
    _, _ -> A.snoc acc segs

type Ctx r
  = (Compiler r)

makeComponent ∷ { | Ctx () } -> Effect (ReactComponent { code ∷ String })
makeComponent { compileAndRun } = do
  ic <- InlineCode.makeComponent
  modal <- Modal.makeComponent
  closeIcon <- CloseIcon.makeComponent
  cluster <- Cluster.makeComponent
  clickAway <- ClickAway.makeComponent
  button <- mkButton
  component "FillInTheGaps" \{ code } -> React.do
    let
      lines = split (Pattern "\n") code

      initialSegments = smooshFillers (lines <#> \line -> rawSegments line <#> toSegment)
    segments /\ modifySegments <- useState initialSegments
    result /\ modifyResult <- useState Nothing
    cssTheme <- useTheme
    let
      highlighterTheme = mkHighlighterTheme cssTheme
    modalTransitions <-
      useTransition [ result ] (Just show)
        $ css
            { from: { opacity: 0.0, transform: "translate3d(-50%, -50%, 0) scale3d(0.3, 0.3, 1.0)" }
            , enter: { opacity: 1.0, transform: "translate3d(-50%, -50%, 0) scale3d(1.0, 1.0, 1.0)" }
            , leave: { opacity: 0.0, transform: "translate3d(-50%, -50%, 0) scale3d(0.3, 0.3, 1.0)" }
            , config:
              mkFn2 \_ state -> case state of
                "leave" -> { mass: 1.0, tension: 140, friction: 15 }
                _ -> { mass: 1.0, tension: 170, friction: 20 }
            }
    clickAwayTransitions <-
      useTransition [ result ] (Just show)
        $ css
            { from: { opacity: 0.0 }
            , enter: { opacity: 1.0 }
            , leave: { opacity: 0.0 }
            , config:
              mkFn2 \_ state -> case state of
                "leave" -> { mass: 1.0, tension: 140, friction: 15 }
                _ -> { mass: 1.0, tension: 170, friction: 20 }
            }
    let
      expectedResult = findResult (join segments)

      onClick =
        launchAff_ do
          do
            res <- compileAndRun { code: toCode segments }
            modifyResult (const (Just $ res)) # liftEffect
    pure
      $ fragment
      $ [ renderSegments highlighterTheme ic (modifySegments) segments
        , jsx cluster {}
            [ R.div_
                [ jsx button
                    { onClick: handler_ onClick
                    , buttonType:
                      if complete segments && isNothing result then
                        HighlightedButton
                      else
                        DisabledButton
                    }
                    [ R.text "Incantate" ]
                ]
            ]
        , fragment
            $ clickAwayTransitions
            >>= \{ item, key, props } ->
                guard (isJust item)
                  $ join item
                  # foldMap \_ ->
                      [ element clickAway (justifill { allowEscape: true, style: props, onClick: modifyResult (const Nothing) }) ]
        , fragment
            $ modalTransitions
            >>= \{ item, key, props } ->
                guard (isJust item)
                  $ join item
                  # foldMap \r ->
                      [ jsx modal
                          { title:
                            case r of
                              (Right { stdout })
                                | S.stripSuffix (S.Pattern "\n") stdout == Just expectedResult -> "Hooray!"
                              (Left l) -> "Does not compile"
                              (Right _) -> "Not " <> (findResult $ join segments)
                          , icon: element closeIcon { onClick: modifyResult (const Nothing), style: Nothing }
                          , style: props
                          }
                          [ R.pre_ [ R.text $ compileResultToString result ] ]
                      ]
        ]

compileResultToString ∷ Maybe (Either CompileResult RunResult) -> String
compileResultToString = case _ of
  Nothing -> ""
  Just (Left cr) -> cr.result <#> _.message # intercalate "\n"
  Just (Right r) -> r.stdout
