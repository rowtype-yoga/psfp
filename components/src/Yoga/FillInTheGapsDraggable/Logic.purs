module Yoga.FillInTheGapsDraggable.Logic where

import Prelude
import Data.Array (intercalate)
import Data.Array as A
import Data.Foldable (foldl)
import Data.Lens ((%~))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String as S
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafeCrashWith)
import Yoga.Helpers ((?||))

complete ∷ Array (Array Segment) -> Boolean
complete arr = foldl f true (join arr)
  where
  f acc seg =
    acc
      && case seg of
          Hole _ s -> s /= ""
          _ -> true

data Segment
  = ExpectedResult String
  | Start
  | End
  | Filler String
  | Hole Int String

derive instance eqSegment ∷ Eq Segment
derive instance ordSegment ∷ Ord Segment
isHole ∷ Segment -> Boolean
isHole = case _ of
  Hole _ _ -> true
  _ -> false

holeToFiller ∷ Segment -> Segment
holeToFiller = case _ of
  Hole _ text -> Filler text
  other -> other

getResult ∷ Segment -> Maybe String
getResult = case _ of
  ExpectedResult r -> Just $ S.replaceAll (S.Pattern ("\\n")) (S.Replacement "\n") r
  _ -> Nothing

findResult ∷ Array Segment -> Maybe String
findResult = A.findMap getResult

findFirstHoleIndex ∷ Array (Array Segment) -> Maybe { i ∷ Int, j ∷ Int }
findFirstHoleIndex lines = do
  i <- A.findIndex (map isHole >>> A.elem true) lines
  line <- lines A.!! i
  j <- A.findIndex isHole line
  pure { i, j }

toCode ∷ Array (Array Segment) -> String
toCode lines = intercalate "\n" mapped
  where
  mapped = lines <#> (intercalate "" <<< map segmentToCode)
  segmentToCode = case _ of
    Filler s -> s
    Hole _ s -> s
    _ -> ""

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

updateSegments ∷ Int -> Int -> String -> Array (Array Segment) -> Array (Array Segment)
updateSegments i j v = (ix i <<< ix j) %~ f
  where
  f = case _ of
    Hole h _ -> Hole h v
    _ -> unsafeCrashWith "Updated a non-hole"

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

parseSegments ∷ String -> Maybe (Array (Array Segment))
parseSegments code = result $> segments
  where
  lines = split (Pattern "\n") code
  segments = smooshFillers (lines <#> \line -> rawSegments line <#> toSegment)
  result = findResult (join segments)
