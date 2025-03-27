{-# LANGUAGE
  GADTs,
  DataKinds,
  StandaloneKindSignatures,
  TypeOperators,
  ScopedTypeVariables,
  TypeFamilies,
  TypeApplications,
  UndecidableInstances,
  TypeSynonymInstances,
  FlexibleInstances,
  OverloadedRecordDot,
  AllowAmbiguousTypes,
  RankNTypes,
  FlexibleContexts
#-}

module Environment where
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Text.Short as TS
import Data.Maybe
import Data.Type.Equality
import Type.Reflection
import Data.Kind
import Data.Tagged
import Data.Proxy
import qualified Control.Monad.Trans.State.Strict as State
import GHC.TypeLits
import GHC.Base (liftM2)
import Language.Haskell.TH (Extension(RankNTypes, FlexibleContexts))
import qualified Data.Text.Short as TS

type EnvProto :: [Type] -> Type
data EnvProto xs where
  EnvNihil :: EnvProto '[]
  (:+:)    :: (x ~ Tagged (a :: Symbol) b, KnownSymbol a, ConstructionVariable b) => x -> EnvProto xs -> EnvProto (x : xs)
infixr 5 :+:

envVar :: forall (s :: Symbol) a . KnownSymbol s => a -> Tagged s a
envVar (x :: a) = Tagged x :: Tagged s a

type LookupType :: Symbol -> [Type] -> Type
type family LookupType s a where
  LookupType s (Tagged s x ': xs) = x
  LookupType s (x ': xs) = LookupType s xs
  LookupType s '[] = TypeError (Text "Unknown environment variable " :<>: ShowType s)

type Append :: [Type] -> [Type] -> [Type]
type family Append l1 l2 where
  Append '[] xs = xs
  Append (y ': ys) xs = Append ys (y ': xs)

eProtoAppend :: EnvProto xs -> EnvProto ys -> EnvProto (Append xs ys)
eProtoAppend EnvNihil e = e
eProtoAppend (x :+: xs) e = eProtoAppend xs (x :+: e)

(+:) = eProtoAppend
infixl 4 +:

data VarHolder = forall a . ConstructionVariable a => VarHolder a
instance Show VarHolder where
  show (VarHolder x) = show x
type ProtoMap = HM.HashMap TS.ShortText VarHolder

castVarHolder :: forall a . ConstructionVariable a => VarHolder -> a
castVarHolder (VarHolder x :: b) = case testEquality (typeOf x) (TypeRep @a) of Just Refl -> x

mergeVarHolder :: VarHolder -> VarHolder -> VarHolder
mergeVarHolder (VarHolder x) (VarHolder y) = case testEquality (typeOf x) (typeOf y) of Just Refl -> VarHolder $ merge x y

tsSymbol :: forall (s :: Symbol) . KnownSymbol s => TS.ShortText
tsSymbol = pack $ symbolVal (Proxy @s)

eProtoMap :: (forall t . (ConstructionVariable t) => t -> VarHolder) -> EnvProto vars -> ProtoMap
eProtoMap _ EnvNihil = HM.empty
eProtoMap m (x :+: next) = HM.insert eName eValue (eProtoMap m next) where
  eTerm (Tagged v :: Tagged s tv) = (tsSymbol @s, m v)
  (eName, eValue) = eTerm x

data Environment vars where
    Environment :: { prototype :: EnvProto vars, defaults :: ProtoMap, overrides :: ProtoMap } -> Environment vars

instance Show (Environment vars) where
  show env = if HM.null env.overrides then "{}" else "{" ++ foldr1 (\x y -> x ++ ", " ++ y) (HM.mapWithKey stringify env.overrides) ++ "}" where
    stringify k v = unpack k ++ ": " ++ show v

makeEnv :: EnvProto vars -> Environment vars
makeEnv proto = Environment proto (eProtoMap VarHolder proto) HM.empty

eLookup :: forall (s :: Symbol) {vars} {a} . (ConstructionVariable a, KnownSymbol s, a ~ LookupType s vars) => Environment vars -> a
eLookup env =
  let
    var = tsSymbol @s
    override = HM.lookup var env.overrides
    value = fromMaybe (fromJust $ HM.lookup var env.defaults) override
  in
    castVarHolder value

eUpdate :: forall (s :: Symbol) {vars} {a} . (ConstructionVariable a, KnownSymbol s, a ~ LookupType s vars) => (a -> Maybe a) -> Environment vars -> Environment vars
eUpdate f env =
  let
    var = tsSymbol @s
    def = castVarHolder @a $ env.defaults HM.! var
    alter v =
      let prev_val = maybe def (castVarHolder @a) v
          val = fromMaybe def (f prev_val)
      in
        if val == def then Nothing else Just $ VarHolder val
    in
      Environment env.prototype env.defaults (HM.alter alter var env.overrides)

eReplace :: forall (s :: Symbol) {vars} {v} . (ConstructionVariable v, KnownSymbol s,  v ~ LookupType s vars) => v -> Environment vars -> Environment vars
eReplace v = eUpdate @s (\_-> Just v)

class (Eq a, Typeable a, Show a) => ConstructionVariable a where
  merge :: a -> a -> a
exclusiveMerge :: Eq a => a -> a -> a
exclusiveMerge a b = if a == b then a else error "conflicting variable values"
instance ConstructionVariable Bool where
  merge = exclusiveMerge
instance ConstructionVariable Int where
  merge = exclusiveMerge
instance ConstructionVariable String where
  merge = exclusiveMerge
instance ConstructionVariable a => ConstructionVariable (Maybe a) where
  merge = (<>)
instance ConstructionVariable [String] where
  merge = (++)

instance {-# OVERLAPPABLE #-} ConstructionVariable a => Semigroup a where
  (<>) = merge

eMerge :: Environment vars -> Environment vars -> Environment vars
eMerge env1 env2 = Environment env1.prototype env1.defaults $ HM.unionWithKey doMerge env1.overrides env2.overrides where
  doMerge k = mergeVarHolder

class EnvTransform a where
  eTransform :: Typeable (Environment vars) => a -> Environment vars -> Environment vars

data EIdentity = EIdentity deriving Show
instance EnvTransform EIdentity where
  eTransform x = id

eDropOverrides :: Environment vars -> Environment vars
eDropOverrides (Environment vars defaults _) = Environment vars defaults HM.empty

data EDropOverrides = EDropOverrides
instance EnvTransform EDropOverrides where
  eTransform x = eDropOverrides

type ETransformer env = State.State env ()
data EStateTransform = forall vars a . (Typeable (Environment vars), a ~ ETransformer (Environment vars)) => EStateTransform a

instance EnvTransform EStateTransform where
  eTransform (EStateTransform (st :: ETransformer env1)) (env :: env2) = case testEquality (TypeRep @env1) (TypeRep @env2) of Just Refl -> State.execState st env