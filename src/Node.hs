{-# LANGUAGE TypeFamilies, BlockArguments #-}

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

data Node where
    ValueNode :: { name :: String } -> Node
    FsNode :: { path :: OsPath } -> Node
    deriving (Eq, Ord, Lift)

instance Show Node where
    show (ValueNode n) = "value:" <> n
    show (FsNode p) = "fs:" <> (force . unsafePerformIO) (decodeFilename p)

instance Read Node where
  readPrec = parens $ prec 10 $ do
    do
        Ident "value" <- lexP
        Symbol ":" <- lexP
        Ident n <- lexP
        return $ ValueNode n
    <|>
    do
        Ident "fs" <- lexP
        Symbol ":" <- lexP
        Ident fname <- lexP
        return $ mkFsNodeFromString fname

instance Hashable Node where
    hashWithSalt salt (FsNode path) = hashWithSalt salt path
    hashWithSalt salt (ValueNode name) = hashWithSalt salt name

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
mkValue = ValueNode
goal :: Node
goal = mkValue "goal"

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
