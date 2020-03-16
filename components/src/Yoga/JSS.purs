module JSS where

import Prelude
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
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
  ∀ theme props r.
  (Homogeneous r (JSSElem props)) =>
  ({ | theme } -> { | r }) -> JSSClasses theme props r
jssClasses = JSSClasses

data JSSElem props
  = PrimitiveJss (String |+| Int |+| Number)
  | ArrayJss (Array (JSSElem props))
  | PropsJss (props -> JSSElem props)
  | NestedJss (Object (JSSElem props))

instance semigroupJssElem ∷ Semigroup (JSSElem props) where
  append = case _, _ of
    NestedJss n1, NestedJss n2 -> NestedJss (n1 <> n2)
    _, second -> second

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
