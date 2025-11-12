{-# LANGUAGE
  DataKinds,
  TypeFamilies,
  UndecidableInstances,
  OverloadedRecordDot,
  AllowAmbiguousTypes,
  RequiredTypeArguments
#-}

module Environment where
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text.Short as TS
import Data.Maybe
import Data.Type.Equality
import Type.Reflection
import Data.Kind
import Data.Tagged
import Data.Proxy
import qualified Control.Monad.Trans.State.Strict as State
import GHC.TypeLits
import GHC.Base (liftM2)
import Text.Read
import Data.Char
import Text.ParserCombinators.ReadP
import Language.Haskell.TH (Extension(RankNTypes, FlexibleContexts, RequiredTypeArguments))
import System.OsString (OsString, encodeLE)
import Data.String
import System.OsPath (encodeFS)
import System.IO.Unsafe (unsafePerformIO)

data VarName = LocalVar Symbol | Symbol :. Symbol

class VarNameVal (n :: VarName) where
  varNameVal :: String

instance KnownSymbol s => VarNameVal (LocalVar s) where
  varNameVal = symbolVal (Proxy @s)

instance (KnownSymbol ns, KnownSymbol v) => VarNameVal (ns :. v) where
  varNameVal = intercalate "." [symbolVal $ Proxy @ns, symbolVal $ Proxy @v]

type EnvProto :: [Type] -> Type
data EnvProto xs where
  EnvNihil :: EnvProto '[]
  (:+:)    :: (x ~ Tagged (a :: VarName) b, VarNameVal a, ConstructionVariable b) => x -> EnvProto xs -> EnvProto (x : xs)
infixr 5 :+:

envVar :: forall (n :: VarName) a . VarNameVal n => a -> Tagged n a
envVar (x :: a) = Tagged x :: Tagged n a

type LookupType :: VarName -> [Type] -> Type
type family LookupType s a where
  LookupType s (Tagged s x ': xs) = x
  LookupType s (x ': xs) = LookupType s xs
  LookupType s '[] = TypeError (Text "Unknown environment variable " :<>: ShowType s)

type Append :: [Type] -> [Type] -> [Type]
type family Append l1 l2 where
  Append '[] xs = xs
  Append (y ': ys) xs = Append ys (y ': xs)

type UseEnv :: [Type] -> [Type] -> Constraint
type family UseEnv toolVars vars where
  UseEnv (Tagged s v  ': tvars) evars = (LookupType s evars ~ v, UseEnv tvars evars)
  UseEnv '[] _ = ()

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

tsSymbol :: forall (n :: VarName) -> VarNameVal n => TS.ShortText
tsSymbol n = TS.pack $ varNameVal @n

eMap :: (forall t . (ConstructionVariable t) => t -> a) -> EnvProto vars -> HM.HashMap TS.ShortText a
eMap _ EnvNihil = HM.empty
eMap m ((x :: Tagged n v) :+: next) = HM.insert (tsSymbol n) (m $ unTagged x) (eMap m next)

eProtoMap :: (forall t . (ConstructionVariable t) => t -> VarHolder) -> EnvProto vars -> ProtoMap
eProtoMap = eMap

readerRegistry :: EnvProto vars -> HM.HashMap TS.ShortText (String -> VarHolder)
readerRegistry = eMap (\(t :: v) -> VarHolder . read @v)

parseVars :: EnvProto vars -> String -> ProtoMap
parseVars proto input = foldr (uncurry HM.insert) HM.empty results where
  [(results, "")] = readP_to_S (varParser proto) input
  varParser proto = sepBy var (skipSpaces >> char '\n') <* eof
  var = do
    skipSpaces
    var_name <- TS.pack <$> munch1 ((||) <$> isAlphaNum <*> (=='.'))
    let reader = case HM.lookup var_name registry of
          Just r -> r
          _      -> error $ "Unknown variable: " <> TS.unpack var_name
    skipSpaces
    char '='
    var_value <- munch1 (/='\n')
    return (var_name, reader var_value)
  registry = readerRegistry proto

writeVars :: ProtoMap -> String
writeVars protomap = intercalate "\n" $ map (\(k, v) -> TS.unpack k <> "=" <> show v) (HM.toList protomap)

data Environment vars where
    Environment :: { prototype :: EnvProto vars, defaults :: ProtoMap, overrides :: ProtoMap } -> Environment vars

instance Show (Environment vars) where
  show env = if HM.null env.overrides then "{}" else "{" ++ foldr1 (\x y -> x ++ ", " ++ y) (HM.mapWithKey stringify env.overrides) ++ "}" where
    stringify k v = TS.unpack k ++ ": " ++ show v

makeEnv :: EnvProto vars -> Environment vars
makeEnv proto = Environment proto (eProtoMap VarHolder proto) HM.empty

eLookup :: forall {vars} {a} . forall (n :: VarName) -> (ConstructionVariable a, VarNameVal n, a ~ LookupType n vars) => Environment vars -> a
eLookup n env =
  let
    var = tsSymbol n
    override = HM.lookup var env.overrides
    value = fromMaybe (fromJust $ HM.lookup var env.defaults) override
  in
    castVarHolder value

eUpdate :: forall {vars} {a} . forall (n :: VarName) -> (ConstructionVariable a, VarNameVal n, a ~ LookupType n vars) => (a -> Maybe a) -> Environment vars -> Environment vars
eUpdate n f env =
  let
    var = tsSymbol n
    def = castVarHolder @a $ env.defaults HM.! var
    alter v =
      let prev_val = maybe def (castVarHolder @a) v
          val = fromMaybe def (f prev_val)
      in
        if val == def then Nothing else Just $ VarHolder val
    in
      Environment env.prototype env.defaults (HM.alter alter var env.overrides)

eReplace :: forall {vars} {v} . forall (n :: VarName) -> (ConstructionVariable v, VarNameVal n, v ~ LookupType n vars) => v -> Environment vars -> Environment vars
eReplace n v = eUpdate n (\_-> Just v)

class (Eq a, Typeable a, Show a, Read a) => ConstructionVariable a where
  merge :: a -> a -> a
exclusiveMerge :: Eq a => a -> a -> a
exclusiveMerge a b = if a == b then a else error "conflicting variable values"
instance ConstructionVariable Bool where
  merge = exclusiveMerge
instance ConstructionVariable Int where
  merge = exclusiveMerge
instance ConstructionVariable a => ConstructionVariable (Maybe a) where
  merge = (<>)
instance {-# OVERLAPPABLE #-} ConstructionVariable a => Semigroup a where
  (<>) = merge

newtype StrVar = StrVar OsString deriving (Eq, Show)
instance ConstructionVariable StrVar where
  merge = exclusiveMerge
instance IsString StrVar where
  fromString = StrVar . unsafePerformIO . encodeFS
instance Read StrVar where
  readPrec = parens $ prec 10 $ do
    Ident "StrVar" <- lexP
    fromString <$> readPrec @String

instance ConstructionVariable [StrVar] where
  merge = (++)

eMerge :: Environment vars -> Environment vars -> Environment vars
eMerge env1 env2 = Environment env1.prototype env1.defaults $ HM.unionWithKey doMerge env1.overrides env2.overrides where
  doMerge k = mergeVarHolder

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

instance Typeable vars => EnvTransform (Environment vars -> Environment vars) where
    eTransform (f :: Environment vars1 -> Environment vars1) (x :: Environment vars2) = case testEquality (TypeRep @vars1) (TypeRep @vars2) of
      Just Refl -> f x
      Nothing -> error $ "Incompatible environments: \n" ++ show (TypeRep @vars1) ++ "\n And\n" ++ show (TypeRep @vars2)

type ETransformer env = State.State env ()
data EStateTransform = forall vars a . (Typeable vars, a ~ ETransformer (Environment vars)) => EStateTransform a

instance EnvTransform EStateTransform where
  eTransform (EStateTransform (st :: ETransformer env1)) (env :: env2) = case testEquality (TypeRep @env1) (TypeRep @env2) of Just Refl -> State.execState st env