module JSS where

import Prelude
import CSS (class Val, App(..), BackgroundImage, Color, Key(..), Keyframes(..), Path(..), Predicate(..), Refinement(..), Rule(..), Selector(..), StyleM, Value(..), cssStringRGBA, plain, predicate, runS, value)
import Data.Foldable (foldMap)
import Data.Int (round)
import Data.Newtype (un, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Foreign (unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.RowList (class RowToList, Nil, kind RowList)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (class WriteForeign, write, writeImpl)
import Type.Prelude (RLProxy, Proxy(..))
import Type.Row as R
import Type.Row.Homogeneous (class Homogeneous)
import Type.RowList as RL
import Untagged.Coercible (coerce)
import Untagged.Union (type (|+|))

newtype JSSClasses theme props r
  = JSSClasses ({ | theme } -> { | r })

jssClasses ∷
  ∀ theme props row rowRL to.
  JSSAbleFields props rowRL row () to =>
  RowToList row rowRL =>
  Homogeneous to (JSSElem props) =>
  JSSAble props (Record row) =>
  ({ | theme } -> { | row }) -> JSSClasses theme props to
jssClasses f = JSSClasses f'
  where
  f' theme = built
    where
    rec = f theme
    built ∷ Record to
    built = Builder.build builder {}
    rlp = RL.RLProxy ∷ RL.RLProxy rowRL
    propsy = Proxy ∷ Proxy props
    builder ∷ Builder.Builder (Record ()) (Record to)
    builder = jssifyFields propsy rlp rec

data JSSElem props
  = PrimitiveJss (String |+| Int |+| Number)
  | ArrayJss (Array (JSSElem props))
  | PropsJss (props -> JSSElem props)
  | NestedJss (Object (JSSElem props))

instance semigroupJssElem ∷ Semigroup (JSSElem props) where
  append = case _, _ of
    PrimitiveJss p1, PrimitiveJss p2
      | p1 == p2 -> PrimitiveJss p1
    NestedJss n1, NestedJss n2 -> NestedJss (n1 <> n2)
    ArrayJss a1, ArrayJss a2 -> ArrayJss (a1 <> a2)
    NestedJss n1, PropsJss fn ->
      PropsJss \props -> case fn props of
        NestedJss n2 -> NestedJss (n1 <> n2)
        other -> do
          let
            _ = spy "other" other
          unsafeCrashWith "Untreated branch (1), fix me"
    PropsJss fn1, PropsJss fn2 ->
      PropsJss \props -> do
        case fn1 props, fn2 props of
          n1@(NestedJss _), n2@(NestedJss _) -> n1 <> n2
          p1@(PrimitiveJss _), p2@(PrimitiveJss _) -> p1 <> p2
          x, y -> do
            let
              _ = spy "x" x

              _ = spy "y" y
            unsafeCrashWith "Untreated branch (2), fix me"
    x, y -> do
      let
        _ = spy "x" x

        _ = spy "y" y
      unsafeCrashWith "Untreated branch (3), fix me"

instance wfJSSElem ∷ WriteForeign (JSSElem props) where
  writeImpl = case _ of
    PrimitiveJss p -> unsafeToForeign p
    ArrayJss proper -> write (map writeImpl proper)
    PropsJss proper -> unsafeToForeign (map writeImpl proper)
    NestedJss proper -> writeImpl (map writeImpl proper)

class JSSAble p a | a -> p where
  jss ∷ a -> JSSElem p

instance jssAbleJssElem ∷ JSSAble p (JSSElem p) where
  jss = identity

instance jssAbleColor ∷ JSSAble p Color where
  jss c = PrimitiveJss (coerce (cssStringRGBA c))

instance jssAbleBackgroundImage ∷ JSSAble p BackgroundImage where
  jss v = PrimitiveJss (coerce (render (value v)))
    where
    render (Value val) = plain val

instance jssAbleString ∷ JSSAble p String where
  jss = coerce >>> PrimitiveJss

instance jssAbleInt ∷ JSSAble p Int where
  jss = coerce >>> PrimitiveJss

instance jssAbleNumber ∷ JSSAble p Number where
  jss = coerce >>> PrimitiveJss

instance jssAbleArray ∷ JSSAble p a => JSSAble p (Array a) where
  jss arr = ArrayJss (jss <$> arr)

instance jssAbleWithProps ∷ JSSAble p jss => JSSAble p (p -> jss) where
  jss = propsJss

instance jssAbleNested ∷ JSSAble p jss => JSSAble p (Object jss) where
  jss = map jss >>> NestedJss

instance jssAbleCss ∷ JSSAble p (StyleM Unit) where
  jss someCss = NestedJss (foldMap ruleToObject rules)
    where
    rules = runS someCss
    ruleToObject = case _ of
      Property (Key k) (Value v) -> Object.singleton (plain k) (jss (plain v))
      Nested (Sub (Selector (Refinement preds) (Elem el))) rules -> Object.singleton (el <> foldMap predicate preds) (jss (foldMap ruleToObject rules))
      Keyframe (Keyframes name frames) ->
        frames
          # foldMap \(Tuple pos rules) ->
              Object.singleton
                (show (round pos) <> "%")
                (NestedJss (foldMap ruleToObject rules))
      x ->
        unsafeCrashWith do
          let
            _ = spy "Unsupported CSS" x
          "tried to create CSS that I couldn't understand"

instance jssAbleRecord ∷
  ( JSSAbleFields props rowRL row () to
  , RowToList row rowRL
  , Homogeneous to (JSSElem props)
  ) =>
  JSSAble props (Record row) where
  jss rec = NestedJss <<< Object.fromHomogeneous $ built
    where
    built ∷ Record to
    built = Builder.build builder {}
    rlp = RL.RLProxy ∷ RL.RLProxy rowRL
    propsy = Proxy ∷ Proxy props
    builder ∷ Builder.Builder (Record ()) (Record to)
    builder = jssifyFields propsy rlp rec

propsJss ∷ ∀ p a. JSSAble p a => (p -> a) -> JSSElem p
propsJss fn = PropsJss (map jss fn)

class JSSAbleFields props (xs ∷ RowList) (row ∷ #Type) (from ∷ #Type) (to ∷ #Type) | xs -> props row from to where
  jssifyFields ∷ Proxy props -> RLProxy xs -> Record row -> Builder (Record from) (Record to)

instance jssAbleFieldsNil ∷ JSSAbleFields props Nil row () () where
  jssifyFields _ _ _ = identity

instance jssAbleFieldsCons ∷
  ( IsSymbol name
  , JSSAble props a
  , R.Lacks name from'
  , R.Cons name a trash row
  , R.Cons name (JSSElem props) from' to
  , JSSAbleFields props tail row from from'
  ) =>
  JSSAbleFields props (RL.Cons name a tail) row from to where
  jssifyFields _ _ r = first <<< rest
    where
    first = Builder.insert nameP (jss val)
    val = Record.get nameP r
    rest = jssifyFields propsP tailP r
    nameP = SProxy ∷ SProxy name
    tailP = RL.RLProxy ∷ RL.RLProxy tail
    propsP = Proxy ∷ Proxy props
    name = reflectSymbol nameP
