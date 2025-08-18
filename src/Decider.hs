{-# LANGUAGE BlockArguments #-}
module Decider where

import Data.Int (Int32, Int64)
import Data.Maybe
import Control.Monad
import Control.Exception
import Data.Type.Coercion
import Database.SQLite.Simple
import qualified Data.Text as T
import qualified Data.ByteString as B
import System.OsPath
import System.OsString ( coercionToPlatformTypes )
import System.File.OsPath ( readFile' )
import System.Posix.Files.PosixString
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX
import Crypto.Hash.MD5
import System.IO.Unsafe

import Node
import Db

data DeciderContext = DeciderContext {
    conn :: Connection
}

data MetaData where
    MetaData    :: { timestamp :: Int64, signature :: B.ByteString } -> MetaData
    ValMetaData :: { signature :: B.ByteString } -> MetaData
    Nonexistent :: {} -> MetaData
    deriving (Eq, Show)

nodeChanged :: MetaData -> MetaData -> Ruling
MetaData t1 s1 `nodeChanged` MetaData t2 s2 | t1 == t2 || s1 == s2 = Unchanged
ValMetaData s1 `nodeChanged` ValMetaData s2 | s1 == s2             = Unchanged
Nonexistent    `nodeChanged` Nonexistent                           = Unchanged
_              `nodeChanged` _                                     = Changed

dbExists :: MetaData -> Bool
dbExists Nonexistent = False
dbExists _           = True

dbTimestamp :: MetaData -> Int64
dbTimestamp (MetaData ts _) = ts
dbTimestamp _               = 0

timestampMatch :: MetaData -> MetaData -> Bool
timestampMatch (MetaData t1 _) (MetaData t2 _) | t1 == t2 = True
timestampMatch _               _                          = False

dbSignature :: MetaData -> B.ByteString
dbSignature Nonexistent = B.empty
dbSignature metadata    = metadata.signature

withDeciderContext :: FilePath -> (DeciderContext -> IO a) -> IO a
withDeciderContext dbFile = bracket
    do fmap DeciderContext $ openDb dbFile
    do close . (.conn)

dbName :: Node -> IO (T.Text, T.Text)
dbName (FsNode path)        = do
                                p <- decodeFS path
                                pure (T.pack "fs", T.pack p)
dbName (ValueNode name _ _) =   pure (T.pack "value", T.pack name)

fromDb :: Nodes -> MetaData
fromDb (Nodes _ _ _ nodeType name existed timestamp signature _ _)
    | nodeType == T.pack "value" = ValMetaData signature
    | existed == False           = Nonexistent
    | otherwise                  = MetaData timestamp signature

data Ruling = Unchanged | Changed deriving (Show, Eq)

instance Semigroup Ruling where
    Changed   <> _ = Changed
    Unchanged <> x = x

instance Monoid Ruling where
    mempty = Unchanged

decideNode :: DeciderContext -> Node -> IO Ruling
decideNode context node = do
    (name, dbtype) <- dbName node
    prevNode <- getNodeInfo context.conn dbtype name
    let prevMetaData = fromDb <$> prevNode
    newMetadata <- buildNewMetadata node
    let changed = fromMaybe Changed $ nodeChanged <$> prevMetaData <*> Just newMetadata
    unless (or $ timestampMatch <$> prevMetaData <*> Just newMetadata) do
        case prevNode of
            Nothing -> initNodeInfo   context.conn dbtype name (dbExists newMetadata) (dbTimestamp newMetadata) (dbSignature newMetadata) Nothing Nothing
            Just ni -> updateNodeInfo context.conn ni          (dbExists newMetadata) (dbTimestamp newMetadata) (dbSignature newMetadata) Nothing Nothing
    return changed

needsRebuild :: Node -> IO Bool
needsRebuild (FsNode path) = do
    fmap not $ fileExist $ toPosix path
needsRebuild (ValueNode {}) = return False

buildNewMetadata :: Node -> IO MetaData
buildNewMetadata (FsNode path) = do
    exists <- fileExist $ toPosix path
    case exists of
        True -> do
            fStatus <- getFileStatus $ toPosix path
            signature <- unsafeInterleaveIO do
                hash <$> readFile' path
            return $ MetaData (mkTimestamp $ modificationTimeHiRes fStatus) signature
        False -> return Nonexistent
buildNewMetadata (ValueNode {}) = return $ ValMetaData B.empty

toPosix path = case coercionToPlatformTypes of
    Right (_, coercion) -> coerceWith coercion path

gainTimestamp :: IO Int64
gainTimestamp = do
    t <- getPOSIXTime
    return $ mkTimestamp t

mkTimestamp t = floor $ ((nominalDiffTimeToSeconds t) * 1e9)
