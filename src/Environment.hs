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
  RankNTypes
#-}

module Environment where
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Dynamic
import Data.Kind
import Data.Tagged
import Data.Proxy
import qualified Control.Monad.Trans.State.Strict as State
import GHC.TypeLits
import GHC.Base (liftM2)
import Language.Haskell.TH (Extension(RankNTypes))

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

type ProtoMap = HM.HashMap String Dynamic

eProtoMap :: (forall t . (ConstructionVariable t) => t -> Dynamic) -> EnvProto vars -> ProtoMap
eProtoMap _ EnvNihil = HM.empty
eProtoMap m (x :+: next) = HM.insert eName eValue (eProtoMap m next) where
  eTerm (Tagged v :: Tagged s tv) = (symbolVal (Proxy @s), m v)
  (eName, eValue) = eTerm x

data Environment vars where
    Environment :: { prototype :: EnvProto vars, defaults :: ProtoMap, overrides :: ProtoMap } -> Environment vars

instance Show (Environment vars) where
  show env = if HM.null env.overrides then "{}" else "{" ++ foldr1 (\x y -> x ++ ", " ++ y) (HM.mapWithKey stringify env.overrides) ++ "}" where
    stringify k v = k ++ ": " ++ (fromJust . fromDynamic @String) (dynApp (dyn_show k) v)
    dyn_show k = proto_show HM.! k
    proto_show = eProtoMap (\(x :: t) -> toDyn (show @t)) env.prototype

makeEnv :: EnvProto vars -> Environment vars
makeEnv proto = Environment proto (eProtoMap toDyn proto) HM.empty

eLookup :: forall (s :: Symbol) {vars} {a} . (ConstructionVariable a, KnownSymbol s, a ~ LookupType s vars) => Environment vars -> a
eLookup env =
  let
    var = symbolVal (Proxy @s)
    override = HM.lookup var env.overrides
    value = fromMaybe (fromJust $ HM.lookup var env.defaults) override
  in
    fromJust $ fromDynamic value

eUpdate :: forall (s :: Symbol) {vars} {a} . (ConstructionVariable a, KnownSymbol s, a ~ LookupType s vars) => (a -> Maybe a) -> Environment vars -> Environment vars
eUpdate f env =
  let
    var = symbolVal (Proxy @s)
    override = HM.lookup var env.overrides
    def = env.defaults HM.! var
    alter_internal (def::a) (v::a) =
      let val = fromMaybe def (f v)
      in
        if val == def then Nothing else Just val
    alter dv = fmap toDyn ((fromJust . fromDynamic @(Maybe a)) (dynApp (dynApp (toDyn alter_internal) def) (fromMaybe def dv)))
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
  merge = liftM2 merge
instance ConstructionVariable [String] where
  merge = (++)

eMerge :: Environment vars -> Environment vars -> Environment vars
eMerge env1 env2 = Environment env1.prototype env1.defaults $ HM.unionWithKey doMerge env1.overrides env2.overrides where
  merger = eProtoMap (\(x :: t) -> toDyn (merge :: t->t->t)) env1.prototype
  doMerge k = dynApp . dynApp (merger HM.! k)

class EnvTransform a where
  eTransform :: Typeable vars => a -> Environment vars -> Environment vars

data EIdentity = EIdentity deriving Show
instance EnvTransform EIdentity where
  eTransform x = id

eDropOverrides :: Environment vars -> Environment vars
eDropOverrides (Environment vars defaults _) = Environment vars defaults HM.empty

data EDropOverrides = EDropOverrides
instance EnvTransform EDropOverrides where
  eTransform x = eDropOverrides

newtype EStateTransform = EStateTransform Dynamic
mkStateTransform :: Typeable vars => State.State (Environment vars) () -> EStateTransform
mkStateTransform st = EStateTransform (toDyn (execST st)) where
  execST = State.execState
instance EnvTransform EStateTransform where
  eTransform (EStateTransform dyn) env =
    fromMaybe
      (error "Incompatible environments")
      (fromDynamic (dynApp dyn (toDyn env)))