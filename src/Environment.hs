{-# LANGUAGE
  DataKinds,
  TypeFamilies,
  UndecidableInstances,
  OverloadedRecordDot,
  AllowAmbiguousTypes,
  RequiredTypeArguments,
  ImplicitParams
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
import GHC.Base (liftM2, Alternative ((<|>)))
import GHC.IsList
import Text.Read
import Data.Char
import Text.ParserCombinators.ReadP
import Language.Haskell.TH (Extension(RankNTypes, FlexibleContexts, RequiredTypeArguments))
import System.OsString (OsString, encodeLE)
import Data.String
import System.OsPath (encodeFS, decodeFS)
import System.IO.Unsafe (unsafePerformIO)

import Node (Node (ValueNode), mkValue)
import Value

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
readerRegistry = eMap (\(t :: v) -> VarHolder . readFromCmdLine @v)

readVars :: EnvProto vars -> [(String, String)] -> ProtoMap
readVars proto = foldr read_var HM.empty where
  read_var (name, value) = let n = TS.pack name in HM.insert n (reader n value)
  registry = readerRegistry proto
  reader name = case HM.lookup name registry of
    Just r -> r
    _      -> error $ "Unknown variable: " <> TS.unpack name

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

eInsertTS :: forall {vars} {v} {a} . forall (n :: VarName) -> (ConstructionVariable v, VarNameVal n, v ~ LookupType n vars, v ~ TSList a, ?target::Node) => [a] -> Environment vars -> Environment vars
eInsertTS n val = eUpdate n (Just . insertTS ?target val)

class (Eq a, Typeable a, Show a, Read a) => ConstructionVariable a where
  merge :: a -> a -> a
  fromCmdLine :: ReadP a
  fromCmdLine = readPrec_to_P (readPrec @a) 0
exclusiveMerge :: Eq a => a -> a -> a
exclusiveMerge a b = if a == b then a else error "conflicting variable values"
readFromCmdLine :: forall a . ConstructionVariable a => String -> a
readFromCmdLine s = case readP_to_S (fromCmdLine @a <* eof) s of
  (result, ""):_ -> result
instance ConstructionVariable Bool where
  merge = exclusiveMerge
instance ConstructionVariable Int where
  merge = exclusiveMerge
instance ConstructionVariable a => ConstructionVariable (Maybe a) where
  merge x y = liftA2 merge x y <|> x <|> y
  fromCmdLine = Nothing <$ eof <|> Just <$> fromCmdLine @a

newtype StrVar = StrVar { unStrVar :: OsString } deriving (Eq, Show, Semigroup)
instance ConstructionVariable StrVar where
  merge = exclusiveMerge
  fromCmdLine = fromString <$> munch (const True)
instance IsString StrVar where
  fromString = StrVar . unsafePerformIO . encodeFS
toString :: StrVar -> String
toString = unsafePerformIO . decodeFS . (.unStrVar)
instance Read StrVar where
  readPrec = parens $ prec 10 $ do
    Ident "StrVar" <- lexP
    fromString <$> readPrec @String

instance Value StrVar where
    toCmdLine (StrVar a) = [a]

newtype TSList a = TSList [(Node, [a])] deriving (Eq, Show, Read, IsList)

instance Value a => Value (TSList a) where
    toCmdLine (TSList ((x,a):xs)) = toCmdLine a <> toCmdLine (TSList xs)
    toCmdLine (TSList []) = []

insertTS :: Node -> [a] -> TSList a -> TSList a
insertTS n a (TSList xs) = TSList ((n,a):xs)

instance (ConstructionVariable a) => ConstructionVariable (TSList a) where
  merge (TSList xs) (TSList ys) = TSList $ go xs ys common where
    common = map fst xs `intersect` map fst ys
    go ((x,a):xs) ((y,b):ys) (c:cs) = case (x == c, y == c) of
      (True, True) -> (x,a):go xs ys cs
      (True, False) -> (y,b):go ((x,a):xs) ys (c:cs)
      (False, True) -> (x,a):go xs ((y,b):ys) (c:cs)
      (False, False) -> if x < y then
        (x,a):go xs ((y,b):ys) (c:cs)
        else
        (y,b):go ((x,a):xs) ys (c:cs)
    go xs yx [] = sortBy (\(x,_) (y,_) -> compare x y) xs <> yx
  fromCmdLine = TSList [] <$ eof
            <|> mkTSList <$> (string "list:" *> sepBy (munch (/=';')) (char ';'))
            <|> mkTSList <$> sepBy (munch (not . isSpace)) skipSpaces
    where mkTSList = TSList . (:[]) . (mkValue "user-override",) . fmap readFromCmdLine

instance Foldable TSList where
  foldMap f (TSList xs) = foldMap f $ concatMap snd xs

eMerge :: Environment vars -> Environment vars -> Environment vars
eMerge env1 env2 = Environment env1.prototype env1.defaults $ HM.unionWithKey doMerge env1.overrides env2.overrides where
  doMerge k = mergeVarHolder
