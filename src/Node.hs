{-# LANGUAGE TypeFamilies, BlockArguments, RequiredTypeArguments, TemplateHaskellQuotes #-}

module Node where
import System.OsPath
import System.Directory.OsPath
import Algebra.Graph.AdjacencyMap
import GHC.IO (unsafePerformIO)
import Data.Hashable ( Hashable(hashWithSalt) )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH.Syntax ( Lift(lift, liftTyped) )
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.Foldable1 (Foldable1 (foldMap1))
import qualified Data.List.NonEmpty as NE
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.DeepSeq ( force )
import Text.Read hiding (lift)
import Control.Applicative (Alternative((<|>)))
import Type.Reflection
import Data.Kind

import Value

data ValueTyHolder = forall a . (Value a) => ValueTyHolder (TypeRep a)
deriving instance Show ValueTyHolder
instance Eq ValueTyHolder where
    (ValueTyHolder a1) == (ValueTyHolder a2) = SomeTypeRep a1 == SomeTypeRep a2
instance Ord ValueTyHolder where
    compare (ValueTyHolder a1) (ValueTyHolder a2) = compare (SomeTypeRep a1) (SomeTypeRep a2)
instance Hashable ValueTyHolder where
    hashWithSalt salt (ValueTyHolder a) = hashWithSalt salt a
instance Lift ValueTyHolder where
    liftTyped (ValueTyHolder _) = [||ValueTyHolder (typeRep @())||]

mkTyHolder :: forall (ty :: Type) -> (Typeable ty, Value ty) => ValueTyHolder
mkTyHolder ty = ValueTyHolder (typeRep @ty)

mkUnitTyHolder :: ValueTyHolder
mkUnitTyHolder = ValueTyHolder (typeRep @())

data Node where
    ValueNode :: { name :: String, ty :: ValueTyHolder } -> Node
    FsNode :: { path :: OsPath } -> Node
    deriving (Eq, Ord, Lift)

valueTypeSuffix :: forall (a :: Type). TypeRep a -> String
valueTypeSuffix t = if SomeTypeRep t /= SomeTypeRep (typeRep @()) then "!" <> show t else ""

instance Show Node where
    show (ValueNode n (ValueTyHolder t)) = "value:" <> n <> valueTypeSuffix t
    show (FsNode p) = "fs:" <> (force . unsafePerformIO) (decodeFilename p)

instance Read Node where
  readPrec = parens $ prec 10 $ do
    do
        Ident "value" <- lexP
        Symbol ":" <- lexP
        Ident n <- lexP
        return $ ValueNode n mkUnitTyHolder
    <|>
    do
        Ident "fs" <- lexP
        Symbol ":" <- lexP
        Ident fname <- lexP
        return $ mkFsNodeFromString fname

instance Hashable Node where
    hashWithSalt salt (FsNode path) = hashWithSalt salt path
    hashWithSalt salt (ValueNode name ty) = salt `hashWithSalt` name `hashWithSalt` ty

class NodeList l where
    toList :: l -> [Node]

instance NodeList Node where
    toList = (:[])

instance Foldable f => NodeList (f Node) where
    toList = concatMap (:[])

class NodeList l => NodeListNonEmpty l where
    toNonEmpty :: l -> NonEmpty Node

instance NodeListNonEmpty Node where
    toNonEmpty =  (:|[])

instance Foldable1 f => NodeListNonEmpty (f Node) where
    toNonEmpty = foldMap1 (:|[])

instance Value Node where
    toCmdLine (FsNode f) = [f]
    toCmdLine (ValueNode v _) = [encodeVal v]

encodeFilename :: (MonadIO m, MonadFail m) => FilePath -> m OsPath
encodeFilename fn = do
    p <- liftIO $ encodeFS fn
    unless (isValid p) do
        fail $ "Invalid file path: " ++ show p
    return p

decodeFilename :: (MonadIO m) => OsPath -> m String
decodeFilename fn = do
    liftIO $ decodeFS fn

resolveTarget :: AdjacencyMap Node -> FilePath -> Node
resolveTarget graph target = node where
    f_node = mkFsNode (unsafePerformIO $ encodeFilename target)
    node | hasVertex f_node graph = f_node
         | otherwise = error $ "Don't know how to build target: " ++ target

{-# NOINLINE baseDir #-}
baseDir :: OsPath
baseDir = unsafePerformIO . canonicalizePath . unsafeEncodeUtf $ "."
mkFsNode :: OsPath -> Node
mkFsNode = FsNode . makeRelative baseDir . unsafePerformIO . canonicalizePath
mkFsNodeFromString :: FilePath -> Node
mkFsNodeFromString = mkFsNode . force . unsafePerformIO . encodeFilename
mkValue :: String -> Node
mkValue n = ValueNode n mkUnitTyHolder
goal :: Node
goal = mkValue "goal"

nodePath :: Node -> OsPath
nodePath (FsNode a) = a
nodePath a = error $ "FsNode expected instead of " <> show a
nodePathString :: Node -> String
nodePathString = force . unsafePerformIO . decodeFilename . nodePath

fs :: QuasiQuoter
fs = QuasiQuoter {
    quoteExp = \x -> do
        file' <- encodeFilename x
        lift $ mkFsNode file',
    quotePat = \_ -> fail "filelist quasiquoter doesn't support use as pattern",
    quoteType = \_ -> fail "filelist quasiquoter doesn't support use as type",
    quoteDec = \_ -> fail "filelist quasiquoter doesn't support use as declaration"
}

filelist :: QuasiQuoter
filelist = QuasiQuoter {
    quoteExp = \x -> do
        filelist' <- mapM encodeFilename (words x)
        if null filelist' then
            lift ([] :: [Node])
        else
            let files = fromList filelist' in
            lift $ NE.map mkFsNode files,
    quotePat = \_ -> fail "filelist quasiquoter doesn't support use as pattern",
    quoteType = \_ -> fail "filelist quasiquoter doesn't support use as type",
    quoteDec = \_ -> fail "filelist quasiquoter doesn't support use as declaration"
}
