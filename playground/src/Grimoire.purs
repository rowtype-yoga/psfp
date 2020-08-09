module Grimoire
  ( module Grimoire
  , module Control.Alt
  , module Control.Alternative
  , module Control.Apply
  , module Control.Bind
  , module Control.Lazy
  , module Effect.Aff
  , module Effect
  , module Effect.Class
  , module Control.Monad.Cont.Class
  , module Control.Monad.Cont.Trans
  , module Control.Monad.Error.Class
  , module Control.Monad.Except
  , module Control.Monad.Except.Trans
  , module Control.Monad.List.Trans
  , module Control.Monad.Maybe.Trans
  , module Control.Monad.RWS
  , module Control.Monad.RWS.Trans
  , module Control.Monad.Reader
  , module Control.Monad.Reader.Class
  , module Control.Monad.Reader.Trans
  , module Control.Monad.Rec.Class
  , module Control.Monad.State
  , module Control.Monad.State.Class
  , module Control.Monad.State.Trans
  , module Control.Monad.Trans.Class
  , module Control.Monad.Writer
  , module Control.Monad.Writer.Class
  , module Control.Monad.Writer.Trans
  , module Control.MonadPlus
  , module Control.Semigroupoid
  , module Data.Bifoldable
  , module Data.Bifunctor
  , module Data.Bifunctor.Clown
  , module Data.Bifunctor.Flip
  , module Data.Bifunctor.Join
  , module Data.Bifunctor.Joker
  , module Data.Bifunctor.Product
  , module Data.Bifunctor.Wrap
  , module Data.Bitraversable
  , module Data.Char
  , module Data.Distributive
  , module Data.Either
  , module Data.Either.Nested
  , module Data.Enum
  , module Data.Foldable
  , module Data.Function
  , module Data.Function.Uncurried
  , module Data.Functor
  , module Data.Functor.Invariant
  , module Data.HeytingAlgebra
  , module Data.Identity
  , module Data.Int
  , module Data.Int.Bits
  , module Data.List
  , module Data.List.Lazy
  , module Data.List.ZipList
  , module Data.Map
  , module Data.Maybe
  , module Data.Maybe.First
  , module Data.Maybe.Last
  , module Data.Monoid
  , module Data.Monoid.Additive
  , module Data.Monoid.Conj
  , module Data.Monoid.Disj
  , module Data.Monoid.Dual
  , module Data.Monoid.Endo
  , module Data.Monoid.Multiplicative
  , module Data.NaturalTransformation
  , module Data.Nullable
  , module Data.Ord
  , module Data.Ord.Down
  , module Data.Ord.Max
  , module Data.Ord.Min
  , module Data.Ord.Unsafe
  , module Data.Ordering
  , module Data.Set
  , module Data.String.Regex
  , module Data.String.Unsafe
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Data.Traversable
  , module Debug.Trace
  , module Global
  , module Global.Unsafe
  , module Math
  , module Prelude
  , module Type.Proxy
  , module Unsafe.Coerce
  , module Simple.JSON
  , module Effect.Console
  ) where

import Effect.Console (log, logShow)
import Simple.JSON (
  writeJSON, readJSON, class ReadForeign, class WriteForeign,
  readJSON_, readJSON'
)
import Control.Alt
  ( class Alt
  , alt
  , (<|>)
  )
import Control.Alternative
  ( class Alternative
  )
import Control.Apply
  ( applyFirst
  , applySecond
  , lift2
  , lift3
  , lift4
  , lift5
  )
import Control.Bind
  ( bindFlipped
  , composeKleisli
  , composeKleisliFlipped
  , ifM
  , join
  )
import Control.Lazy
  ( class Lazy
  , defer
  , fix
  )
import Effect
  ( Effect
  , untilE
  , whileE
  , forE
  , foreachE
  )
import Effect.Class
  ( liftEffect
  )
import Effect.Aff
  ( Aff
  , Canceler(Canceler)
  , apathize
  , attempt
  , cancelWith
  , finally
  , forkAff
  , delay
  , launchAff
  , makeAff
  , nonCanceler
  , runAff
  )
import Control.Monad.Cont.Class
  ( class MonadCont
  , callCC
  )
import Control.Monad.Cont.Trans
  ( ContT(ContT)
  , mapContT
  , runContT
  , withContT
  )
import Data.Traversable
  ( class Traversable, traverse, sequence
  , traverseDefault, sequenceDefault
  , for
  , scanl
  , scanr
  , mapAccumL
  , mapAccumR
  )
import Control.Monad.Error.Class
  ( class MonadError
  , catchError
  , catchJust
  , throwError
  )
import Control.Monad.Except
  ( Except
  , except
  , mapExcept
  , runExcept
  , withExcept
  )
import Control.Monad.Except.Trans
  ( ExceptT(ExceptT)
  , mapExceptT
  , runExceptT
  , withExceptT
  )
import Control.Monad.List.Trans
  ( ListT
  -- , catMaybes -- TODO
  -- , cons -- TODO
  -- , drop -- TODO
  -- , dropWhile -- TODO
  -- , filter -- TODO
  -- , foldl -- NOTE: Data.Foldable
  -- , foldl' -- NOTE: Data.Foldable
  , fromEffect
  -- , head -- TODO
  -- , iterate -- TODO
  -- , mapMaybe -- TODO
  -- , nil -- TODO
  , prepend
  , prepend'
  -- , repeat -- TODO
  -- , scanl -- NOTE: Data.Traversable
  -- , singleton -- NOTE: Data.Unfoldable
  -- , tail -- TODO
  -- , take -- TODO
  -- , takeWhile -- TODO
  -- , uncons -- TODO
  , unfold
  , wrapEffect
  , wrapLazy
  -- , zipWith -- TODO
  , zipWith'
  )
import Control.Monad.Maybe.Trans
  ( MaybeT(MaybeT)
  , mapMaybeT
  , runMaybeT
  )
import Control.Monad.RWS
  ( RWS
  , evalRWS
  , execRWS
  , mapRWS
  , runRWS
  , rws
  , withRWS
  )
import Control.Monad.RWS.Trans
  ( RWSResult(RWSResult)
  , RWST(RWST)
  , evalRWST
  , execRWST
  , mapRWST
  , runRWST
  , withRWST
  )
import Control.Monad.Reader
  ( Reader
  , mapReader
  , runReader
  , withReader
  )
import Control.Monad.Reader.Class
  ( class MonadReader
  , class MonadAsk
  , ask
  , asks
  , local
  )
import Control.Monad.Reader.Trans
  ( ReaderT(ReaderT)
  , mapReaderT
  , runReaderT
  , withReaderT
  )
import Control.Monad.Rec.Class
  ( class MonadRec
  , forever
  , tailRec
  , tailRecM
  , tailRecM2
  , tailRecM3
  )
import Control.Monad.State
  ( State
  , evalState
  , execState
  , mapState
  , runState
  , withState
  )
import Control.Monad.State.Class
  ( class MonadState
  , get
  , gets
  , modify
  , put
  , state
  )
import Control.Monad.State.Trans
  ( StateT(StateT)
  , evalStateT
  , execStateT
  , mapStateT
  , runStateT
  , withStateT
  )
import Control.Monad.Trans.Class
  ( class MonadTrans
  , lift
  )
import Control.Monad.Writer
  ( Writer
  , execWriter
  , mapWriter
  , runWriter
  )
import Control.Monad.Writer.Class
  ( class MonadWriter
  , class MonadTell
  , tell
  , censor
  , listen
  , pass
  , listens
  )
import Control.Monad.Writer.Trans
  ( WriterT(WriterT)
  , execWriterT
  , mapWriterT
  , runWriterT
  )
import Control.MonadPlus
  ( class MonadPlus
  , guard
  )
import Control.Semigroupoid
  ( composeFlipped
  )
-- import Data.Array -- TODO
-- import Data.Array.Unsafe -- TODO
import Data.Bifoldable
  ( class Bifoldable
  , biall
  , biany
  , bifold
  , bifoldl
  , bifoldMap
  , bifoldr
  , bifor_
  , bisequence_
  , bitraverse_
  )
import Data.Bifunctor
  ( class Bifunctor
  , lmap
  , rmap
  )
import Data.Bifunctor.Clown
  ( Clown(Clown)
  )
import Data.Bifunctor.Flip
  ( Flip(Flip)
  )
import Data.Bifunctor.Join
  ( Join(Join)
  )
import Data.Bifunctor.Joker
  ( Joker(Joker)
  )
import Data.Bifunctor.Product
  ( Product(Product)
  )
import Data.Bifunctor.Wrap
  ( Wrap(Wrap)
  )
import Data.Bitraversable
  ( class Bitraversable
  , bifor
  , bisequence
  , bitraverse
  )
import Data.Char
  ( fromCharCode
  , toCharCode
  -- , toLower -- NOTE: Data.String
  -- , toUpper -- NOTE: Data.String
  )
import Data.Distributive
  ( class Distributive
  , collect
  , cotraverse
  , distribute
  )
import Data.Either
  ( Either(Left, Right)
  , either
  , isLeft
  , isRight
  )
import Data.Either.Nested
  ( in1
  , in2
  , in3
  , in4
  , in5
  , in6
  , in7
  , in8
  , in9
  , in10
  , at1
  , at2
  , at3
  , at4
  , at5
  , at6
  , at7
  , at8
  , at9
  , at10
  , Either1
  , Either2
  , Either3
  , Either4
  , Either5
  , Either6
  , Either7
  , Either8
  , Either9
  , Either10
  , either1
  , either2
  , either3
  , either4
  , either5
  , either6
  , either7
  , either8
  , either9
  , either10
  )
import Data.Enum
  ( class Enum
  , succ
  , pred
  , defaultSucc
  , defaultPred
  , enumFromTo
  , enumFromThenTo
  , upFrom
  , downFrom
  , Cardinality(..)
  , class BoundedEnum
  , cardinality
  , toEnum
  , fromEnum
  , toEnumWithDefaults
  , defaultCardinality
  , defaultToEnum
  , defaultFromEnum
  )
-- import Data.FingerTree -- FIXME: update upstream
-- ( Digit
-- , FingerTree(Deep, Empty, Single)
-- , LazySplit(LazySplit)
-- , Node(Node2, Node3)
-- , Split(Split)
-- , ViewL(ConsL, NilL)
-- , ViewR(NilR, SnocR)
-- , app3
-- , append -- NOTE: Prelude
-- , compareFingerTree
-- , cons -- TODO
-- , consAll
-- , deep
-- , deepL
-- , deepR
-- , eqFingerTree
-- , filter  -- TODO
-- , fullyForce -- TODO
-- , head -- TODO
-- , headDigit
-- , init -- TODO
-- , initDigit
-- , isEmpty -- TODO
-- , last -- TODO
-- , lastDigit
-- , lazyEmpty
-- , node2
-- , node3
-- , nodes
-- , nodeToDigit
-- , snoc -- TODO
-- , snocAll
-- , split -- NOTE: Data.String.Regex
-- , tail -- TODO
-- , tailDigit
-- , toFingerTree
-- , unfoldLeft
-- , unfoldRight
-- , unsafeSplitDigit
-- , unsafeSplitTree
-- , viewL
-- , viewR
-- )
import Data.Foldable
  ( class Foldable
  , all
  , and
  , any
  , elem
  , find
  , fold
  , foldl
  , foldMap
  , foldr
  , for_
  , intercalate
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , notElem
  , or
  , product
  , sequence_
  , sum
  , traverse_
  )
import Data.Function
  ( applyFlipped
  , on
  )
import Data.Function.Uncurried
  ( Fn0
  , Fn1
  , Fn10
  , Fn2
  , Fn3
  , Fn4
  , Fn5
  , Fn6
  , Fn7
  , Fn8
  , Fn9
  , mkFn0
  , mkFn1
  , mkFn10
  , mkFn2
  , mkFn3
  , mkFn4
  , mkFn5
  , mkFn6
  , mkFn7
  , mkFn8
  , mkFn9
  , runFn0
  , runFn1
  , runFn10
  , runFn2
  , runFn3
  , runFn4
  , runFn5
  , runFn6
  , runFn7
  , runFn8
  , runFn9
  )
import Data.Functor
  ( mapFlipped
  , voidLeft
  , voidRight
  )
import Data.Functor.Invariant
  ( class Invariant
  , imap
  , imapF
  )
import Data.HeytingAlgebra
  ( ff
  , implies
  , tt
  )
import Data.Identity
  ( Identity(Identity)
  )
import Data.Int
  ( ceil
  , even
  , floor
  , fromNumber
  -- , fromString -- TODO
  , odd
  , round
  , toNumber
  )
import Data.Int.Bits
  ( complement
  , shl
  , shr
  , zshr
  , (.&.)
  , (.^.)
  , (.|.)
  )
-- import Data.Lazy
--   ( -- Lazy -- NOTE: Control.Lazy -- , defer -- NOTE: Control.Lazy force
--   )
import Data.List
  ( List(Cons, Nil)
  -- , alterAt -- TODO
  -- , catMaybes -- TODO
  -- , concat -- TODO
  -- , concatMap -- TODO
  -- , delete -- TODO
  -- , deleteAt -- TODO
  -- , deleteBy -- TODO
  -- , drop -- TODO
  -- , dropWhile -- TODO
  -- , elemIndex -- TODO
  -- , elemLastIndex -- TODO
  -- , filter -- TODO
  -- , filterM -- TODO
  -- , findIndex -- TODO
  -- , findLastIndex -- TODO
  -- , foldM -- TODO
  -- , fromList -- TODO
  -- , group -- TODO
  -- , group' -- TODO
  -- , groupBy -- TODO
  -- , head -- TODO
  -- , index -- TODO
  -- , init -- TODO
  -- , insert -- TODO
  -- , insertAt -- TODO
  -- , insertBy -- TODO
  -- , intersect -- TODO
  -- , intersectBy -- TODO
  -- , last -- TODO
  -- , length -- TODO
  -- , many -- TODO
  -- , mapMaybe -- TODO
  -- , modifyAt -- TODO
  -- , nub -- TODO
  -- , nubBy -- TODO
  -- , null -- TODO
  -- , range -- TODO
  -- , replicate -- NOTE: Data.Unfoldable
  -- , replicateM -- TODO
  -- , reverse -- TODO
  -- , singleton -- NOTE: Data.Unfoldable
  -- , slice -- TODO
  -- , snoc -- TODO
  -- , some -- TODO
  -- , sort -- TODO
  -- , sortBy -- TODO
  -- , span -- TODO
  -- , tail -- TODO
  -- , take -- TODO
  -- , takeWhile -- TODO
  -- , toList -- TODO
  -- , uncons -- TODO
  -- , union -- TODO
  -- , unionBy -- TODO
  -- , unzip -- TODO
  -- , updateAt -- TODO
  -- , zip -- TODO
  -- , zipWith -- TODO
  -- , zipWithA -- TODO
  -- , (:) -- TODO
  -- , (!!) -- TODO
  -- , (..) -- TODO
  -- , (\\) -- TODO
  )
import Data.List.Lazy
  ( -- List(List) -- NOTE: Data.List -- , Step(Cons, NIl) -- NOTE: Data.List -- , alterAt -- TODO -- , catMaybes -- TODO
  -- , concat -- TODO
  -- , concatMap -- TODO
  -- , cons -- TODO
  cycle
  -- , delete -- TODO
  -- , deleteAt -- TODO
  -- , deleteBy -- TODO
  -- , drop -- TODO
  -- , dropWhile -- TODO
  -- , filter -- TODO
  -- , fromList -- TODO
  -- , group -- TODO
  -- , groupBy -- TODO
  -- , head -- TODO
  -- , index -- TODO
  -- , init -- TODO
  -- , insert -- TODO
  -- , insertAt -- TODO
  -- , insertBy -- TODO
  -- , intersect -- TODO
  -- , intersectBy -- TODO
  -- , iterate -- TODO
  -- , last -- TODO
  -- , length -- TODO
  -- , mapMaybe -- TODO
  -- , modifyAt -- TODO
  -- , nil -- TODO
  -- , nub -- TODO
  -- , nubBy -- TODO
  -- , null -- TODO
  -- , range -- TODO
  -- , repeat -- TODO
  -- , reverse -- TODO
  -- , singleton -- NOTE: Data.Unfoldable
  -- , span -- TODO
  , step
  -- , tail -- TODO
  -- , take -- TODO
  -- , takeWhile -- TODO
  -- , toList -- TODO
  -- , uncons -- TODO
  -- , union -- TODO
  -- , unionBy -- TODO
  -- , updateAt -- TODO
  -- , zip -- TODO
  -- , zipWith -- TODO
  -- , (:) -- TODO
  -- , (!!) -- TODO
  -- , (..) -- TODO
  -- , (\\) -- TODO
  )
-- import Data.List.Unsafe -- TODO
import Data.List.ZipList
  ( ZipList(ZipList)
  )
import Data.Map
  ( Map
  -- , alter -- TODO
  -- , checkValid -- TODO
  -- , delete -- TODO
  -- , empty -- NOTE: Control.Plus
  -- , fromFoldable -- TODO
  -- , fromFoldableWith -- TODO
  -- , fromList -- TODO
  -- , fromListWith -- TODO
  -- , insert -- TODO
  -- , isEmpty -- TODO
  -- , keys -- TODO
  -- , lookup -- TODO
  -- , member -- TODO
  , showTree
  -- , singleton -- NOTE: Data.Unfoldable
  -- , size -- TODO
  -- , toList -- TODO
  -- , union -- TODO
  -- , unions -- TODO
  , unionWith
  -- , update -- TODO
  -- , values -- TODO
  )
import Data.Maybe
  ( Maybe(Nothing, Just)
  , fromMaybe
  , isJust
  , isNothing
  , maybe
  , maybe'
  )
import Data.Maybe.First
  ( First(First)
  )
import Data.Maybe.Last
  ( Last(Last)
  )
import Data.Monoid
  ( class Monoid
  , mempty
  )
import Data.Monoid.Additive
  ( Additive(Additive)
  )
import Data.Monoid.Conj
  ( Conj(Conj)
  )
import Data.Monoid.Disj
  ( Disj(Disj)
  )
import Data.Monoid.Dual
  ( Dual(Dual)
  )
import Data.Monoid.Endo
  ( Endo(Endo)
  )
import Data.Monoid.Multiplicative
  ( Multiplicative(Multiplicative)
  )
import Data.NaturalTransformation
  ( NaturalTransformation
  )
import Data.Nullable
  ( Nullable
  , toMaybe
  , toNullable
  )
import Data.Ord
  ( abs
  , greaterThan
  , greaterThanOrEq
  , lessThan
  , lessThanOrEq
  , signum
  )
import Data.Ordering
  ( invert
  )
import Data.Ord.Unsafe
  ( unsafeCompare
  )
import Data.Ord.Down
  ( Down(Down)
  )
import Data.Ord.Max
  ( Max(Max)
  )
import Data.Ord.Min
  ( Min(Min)
  )
import Data.Set
  ( Set
  -- , checkValid -- TODO
  -- , delete -- TODO
  , difference
  -- , empty -- NOTE: Control.Plus
  -- , fromList -- TODO
  -- , insert -- TODO
  , intersection
  -- , isEmpty -- TODO
  -- , member -- TODO
  , properSubset
  -- , singleton -- NOTE: Data.Unfoldable
  -- , size -- TODO
  , subset
  -- , toList -- TODO
  -- , union -- TODO
  -- , unions -- TODO
  )
-- import Data.StrMap.Unsafe -- TODO
-- import Data.String.CodeUnits
--   ( stripPrefix
--   , stripSuffix
--   , contains
--   , singleton
--   , fromCharArray
--   , toCharArray
--   , charAt
--   , toChar
--   , uncons
--   , length
--   , countPrefix
--   , indexOf
--   , indexOf'
--   , lastIndexOf
--   , lastIndexOf'
--   , take
--   , takeRight
--   , takeWhile
--   , drop
--   , dropRight
--   , dropWhile
--   , slice
--   , splitAt
--   )
import Data.String.Regex
  ( Regex
  , regex
  , source
  , flags
  , renderFlags
  , parseFlags
  , test
  , match
  , replace
  , replace'
  , search
  , split
  )
import Data.String.Unsafe
  ( char
  -- , charAt -- NOTE: Data.String
  -- , charCodeAt -- NOTE: Data.String
  )
import Data.Tuple
  ( Tuple(Tuple)
  , curry
  , fst
  -- , lookup -- TODO
  , snd
  , swap
  , uncurry
  )
import Data.Tuple.Nested
  ( Tuple10
  , Tuple2
  , Tuple3
  , Tuple4
  , Tuple5
  , Tuple6
  , Tuple7
  , Tuple8
  , Tuple9
  , curry10
  , curry2
  , curry3
  , curry4
  , curry5
  , curry6
  , curry7
  , curry8
  , curry9
  , tuple10
  , tuple2
  , tuple3
  , tuple4
  , tuple5
  , tuple6
  , tuple7
  , tuple8
  , tuple9
  , uncurry10
  , uncurry2
  , uncurry3
  , uncurry4
  , uncurry5
  , uncurry6
  , uncurry7
  , uncurry8
  , uncurry9
  , (/\)
  )
-- import Data.Validation.Sempigroup
--   ( V
--   , invalid
--   , isValid
--   , unV
--   )
import Debug.Trace
  ( spy
  , trace
  )
import Global
  ( decodeURI
  , decodeURIComponent
  , encodeURI
  , encodeURIComponent
  , infinity
  , isFinite
  , isNaN
  , nan
  , readFloat
  -- , readInt -- NOTE: Data.Foreign
  )
import Global.Unsafe
  ( unsafeStringify
  )
import Math
  ( Radians
  -- , abs -- NOTE: Prelude
  , acos
  , asin
  , atan
  , atan2
  -- , ceil -- NOTE: Data.Int
  , cos
  , e
  , exp
  -- , floor -- NOTE: Data.Int
  , ln10
  , ln2
  -- , log
  , log10e
  , log2e
  -- , max -- NOTE: Prelude
  -- , min -- NOTE: Prelude
  , pi
  , pow
  -- , round -- NOTE: Data.Int
  , sin
  , sqrt
  , sqrt1_2
  , sqrt2
  , tan
  , (%)
  )
import Prelude
  ( class Applicative
  , class Apply
  , class Bind
  , class BooleanAlgebra
  , class Bounded
  , class Category
  , class CommutativeRing
  , class Eq
  , class EuclideanRing
  , class Field
  , class Functor
  , class HeytingAlgebra
  , class Monad
  , class Ord
  , class Ring
  , class Semigroup
  , class Semigroupoid
  , class Semiring
  , class Show
  , Ordering(LT, EQ, GT)
  , Unit
  , Void
  , absurd
  , add
  , ap
  , append
  , apply
  , between
  , bind
  , bottom
  , clamp
  , compare
  , comparing
  , compose
  , conj
  , const
  , degree
  , disj
  , div
  , eq
  , flip
  , liftA1
  , liftM1
  , map
  , max
  , min
  , mod
  , mul
  , negate
  , not
  , notEq
  , one
  , otherwise
  , pure
  , show
  , sub
  , top
  , unit
  , unless
  , void
  , zero
  , type (~>)
  , (-)
  , (*)
  , (*>)
  , (/)
  , (/=)
  , (&&)
  , (#)
  , (+)
  , (<)
  , (<*)
  , (<*>)
  , (<#>)
  , (<<<)
  , (<=)
  , (<=<)
  , (<>)
  , (<$)
  , (<$>)
  , (=<<)
  , (==)
  , (>)
  , (>=)
  , (>=>)
  , (>>=)
  , (>>>)
  , (||)
  , ($)
  , ($>)
  )
import Type.Proxy
  ( Proxy(Proxy)
  , Proxy2(Proxy2)
  , Proxy3(Proxy3)
  )
import Unsafe.Coerce
  ( unsafeCoerce
  )
