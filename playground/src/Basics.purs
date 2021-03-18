module Basics
  ( module Basics
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
  , module Data.FunctorWithIndex
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
  , module Data.NonEmpty
  , module Data.Maybe.Last
  , module Data.Monoid
  , module Data.Monoid.Additive
  , module Data.Monoid.Conj
  , module Data.Monoid.Disj
  , module Data.Monoid.Dual
  , module Data.Monoid.Endo
  ,module Data.Array.NonEmpty
  , module Data.Monoid.Multiplicative
  , module Data.NaturalTransformation
  , module Data.Nullable
  , module Data.Ord
  , module Data.Ord.Down
  , module Data.Ord.Max
  , module Data.Ord.Min
  , module Data.Ordering
  , module Data.Set
  , module Data.String.Regex
  , module Data.String.Unsafe
  , module Data.Tuple
  , module Data.Tuple.Nested
  , module Data.Traversable
  , module Debug
  , module Math
  , module Prelude
  , module Type.Proxy
  , module Unsafe.Coerce
  , module Effect.Console
  ) where

import Effect.Console

import Control.Alt (class Alt, alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (applyFirst, applySecond, lift2, lift3, lift4, lift5)
import Control.Bind (bindFlipped, composeKleisli, composeKleisliFlipped, ifM, join)
import Control.Lazy (class Lazy, defer, fix)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.Cont.Trans (ContT(ContT), mapContT, runContT, withContT)
import Control.Monad.Error.Class (class MonadError, catchError, catchJust, throwError)
import Control.Monad.Except (Except, except, mapExcept, runExcept, withExcept)
import Control.Monad.Except.Trans (ExceptT(ExceptT), mapExceptT, runExceptT, withExceptT)
import Control.Monad.List.Trans (ListT, fromEffect, prepend, prepend', unfold, wrapEffect, wrapLazy, zipWith')
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), mapMaybeT, runMaybeT)
import Control.Monad.RWS (RWS, evalRWS, execRWS, mapRWS, runRWS, rws, withRWS)
import Control.Monad.RWS.Trans (RWSResult(RWSResult), RWST(RWST), evalRWST, execRWST, mapRWST, runRWST, withRWST)
import Control.Monad.Reader (Reader, mapReader, runReader, withReader)
import Control.Monad.Reader.Class (class MonadReader, class MonadAsk, ask, asks, local)
import Control.Monad.Reader.Trans (ReaderT(ReaderT), mapReaderT, runReaderT, withReaderT)
import Control.Monad.Rec.Class (class MonadRec, forever, tailRec, tailRecM, tailRecM2, tailRecM3)
import Control.Monad.State (State, evalState, execState, mapState, runState, withState)
import Control.Monad.State.Class (class MonadState, get, gets, modify, put, state)
import Control.Monad.State.Trans (StateT(StateT), evalStateT, execStateT, mapStateT, runStateT, withStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (Writer, execWriter, mapWriter, runWriter)
import Control.Monad.Writer.Class (class MonadWriter, class MonadTell, tell, censor, listen, pass, listens)
import Control.Monad.Writer.Trans (WriterT(WriterT), execWriterT, mapWriterT, runWriterT)
import Control.MonadPlus (class MonadPlus, guard)
import Control.Semigroupoid (composeFlipped)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifoldable (class Bifoldable, biall, biany, bifold, bifoldl, bifoldMap, bifoldr, bifor_, bisequence_, bitraverse_)
import Data.Bifunctor (class Bifunctor, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bifor, bisequence, bitraverse)
import Data.Char (fromCharCode, toCharCode)
import Data.Distributive (class Distributive, collect, cotraverse, distribute)
import Data.Either (Either(Left, Right), either, isLeft, isRight)
import Data.Either.Nested (in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, at1, at2, at3, at4, at5, at6, at7, at8, at9, at10, Either1, Either2, Either3, Either4, Either5, Either6, Either7, Either8, Either9, Either10, either1, either2, either3, either4, either5, either6, either7, either8, either9, either10)
import Data.Enum (class Enum, succ, pred, defaultSucc, defaultPred, enumFromTo, enumFromThenTo, upFrom, downFrom, Cardinality(..), class BoundedEnum, cardinality, toEnum, fromEnum, toEnumWithDefaults, defaultCardinality, defaultToEnum, defaultFromEnum)
import Data.Foldable (class Foldable, all, and, any, elem, find, fold, foldl, foldMap, foldr, for_, intercalate, maximum, maximumBy, minimum, minimumBy, notElem, or, product, sequence_, sum, traverse_)
import Data.Function (applyFlipped, on)
import Data.Function.Uncurried (Fn0, Fn1, Fn10, Fn2, Fn3, Fn4, Fn5, Fn6, Fn7, Fn8, Fn9, mkFn0, mkFn1, mkFn10, mkFn2, mkFn3, mkFn4, mkFn5, mkFn6, mkFn7, mkFn8, mkFn9, runFn0, runFn1, runFn10, runFn2, runFn3, runFn4, runFn5, runFn6, runFn7, runFn8, runFn9)
import Data.Functor (mapFlipped, voidLeft, voidRight)
import Data.Functor.Invariant (class Invariant, imap, imapF)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Identity (Identity(Identity))
import Data.Int (ceil, even, floor, fromNumber, odd, round, toNumber)
import Data.Int.Bits (complement, shl, shr, zshr, (.&.), (.^.), (.|.))
import Data.List (List(Cons, Nil))
import Data.List.Lazy (cycle, step)
import Data.List.ZipList (ZipList(ZipList))
import Data.Map (Map, showTree, unionWith)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isJust, isNothing, maybe, maybe')
import Data.Maybe.First (First(First))
import Data.Maybe.Last (Last(Last))
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(Additive))
import Data.Monoid.Conj (Conj(Conj))
import Data.Monoid.Disj (Disj(Disj))
import Data.Monoid.Dual (Dual(Dual))
import Data.Monoid.Endo (Endo(Endo))
import Data.Monoid.Multiplicative (Multiplicative(Multiplicative))
import Data.NaturalTransformation (NaturalTransformation)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Ord (abs, greaterThan, greaterThanOrEq, lessThan, lessThanOrEq, signum)
import Data.Ord.Down (Down(Down))
import Data.Ord.Max (Max(Max))
import Data.Ord.Min (Min(Min))
import Data.Ordering (invert)
import Data.Set (Set, difference, intersection, properSubset, subset)
import Data.String.Regex (Regex, regex, source, flags, renderFlags, parseFlags, test, match, replace, replace', search, split)
import Data.String.Unsafe (char)
import Data.Traversable (class Traversable, traverse, sequence, traverseDefault, sequenceDefault, for, scanl, scanr, mapAccumL, mapAccumR)
import Data.Tuple (Tuple(Tuple), curry, fst, snd, swap, uncurry)
import Data.Tuple.Nested (Tuple10, Tuple2, Tuple3, Tuple4, Tuple5, Tuple6, Tuple7, Tuple8, Tuple9, curry10, curry2, curry3, curry4, curry5, curry6, curry7, curry8, curry9, tuple10, tuple2, tuple3, tuple4, tuple5, tuple6, tuple7, tuple8, tuple9, uncurry10, uncurry2, uncurry3, uncurry4, uncurry5, uncurry6, uncurry7, uncurry8, uncurry9, (/\))
import Debug (spy, spyWith, trace, traceM)
import Effect (Effect, untilE, whileE, forE, foreachE)
import Effect.Aff (Aff, Canceler(Canceler), apathize, attempt, cancelWith, finally, forkAff, delay, launchAff, makeAff, nonCanceler, runAff)
import Effect.Class (liftEffect)
import Math (Radians, acos, asin, atan, atan2, cos, e, exp, ln10, ln2, log10e, log2e, pi, pow, sin, sqrt, sqrt1_2, sqrt2, tan, (%))
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, Ordering(LT, EQ, GT), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, disj, div, eq, flip, liftA1, liftM1, map, max, min, mod, mul, negate, not, notEq, one, otherwise, pure, show, sub, top, unit, unless, void, zero, type (~>), (-), (*), (*>), (/), (/=), (&&), (#), (+), (<), (<*), (<*>), (<#>), (<<<), (<=), (<=<), (<>), (<$), (<$>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||), ($), ($>))
import Type.Proxy (Proxy(Proxy), Proxy2(Proxy2), Proxy3(Proxy3))
import Unsafe.Coerce (unsafeCoerce)
