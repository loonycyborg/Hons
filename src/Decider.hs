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
import qualified Data.ByteString.Encoding as BE
import qualified Data.HashMap.Strict as HM
import System.OsPath
import System.OsString ( coercionToPlatformTypes )
import System.File.OsPath ( readFile' )
import System.Posix.Files.PosixString
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX
import Crypto.Hash.MD5
import Data.IORef
import System.IO.Unsafe

import Node
import Db

data DeciderContext = DeciderContext {
    dbCache :: IORef (HM.HashMap Node (Maybe Nodes)),
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

skipsDbUpdate :: MetaData -> MetaData -> Bool
skipsDbUpdate (MetaData t1 _)  (MetaData t2 _)  | t1 == t2 = True
skipsDbUpdate (ValMetaData s1) (ValMetaData s2) | s1 == s2 = True
skipsDbUpdate Nonexistent      Nonexistent                 = True
skipsDbUpdate _                _                           = False

dbExists :: MetaData -> Bool
dbExists Nonexistent = False
dbExists _           = True

dbTimestamp :: MetaData -> Int64
dbTimestamp (MetaData ts _) = ts
dbTimestamp _               = 0

dbSignature :: MetaData -> B.ByteString
dbSignature Nonexistent = B.empty
dbSignature metadata    = metadata.signature

withDeciderContext :: FilePath -> (DeciderContext -> IO a) -> IO a
withDeciderContext dbFile = bracket
    do
        r <- newIORef HM.empty
        DeciderContext r <$> openDb dbFile
    do close . (.conn)

dbName :: Node -> (T.Text, T.Text)
dbName (FsNode path) = unsafePerformIO do
                         p <- decodeFS path
                         pure (T.pack "fs",    T.pack p)
dbName (ValueNode name _ _) = (T.pack "value", T.pack name)

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
    (prevMetaData, newMetadata) <- syncDb context node Nothing
    return $ fromMaybe Changed $ nodeChanged <$> prevMetaData <*> Just newMetadata

needsRebuild :: DeciderContext -> Node -> IO Bool
needsRebuild decider node@(FsNode path) = do
    exists <- fileExist $ toPosix path
    prevResult <- fromMaybe False . join <$> fmap (.task_status) <$> getNodeInfoCached decider node
    return $ not prevResult || not exists
needsRebuild decider (ValueNode {}) = return False

wasRebuilt :: DeciderContext -> Node -> Bool -> IO ()
wasRebuilt context node status = do
    void $ syncDb context node $ Just status

syncDb :: DeciderContext -> Node -> Maybe Bool -> IO (Maybe MetaData, MetaData)
syncDb context node status = do
    prevNode <- getNodeInfoCached context node
    let prevMetaData = fromDb <$> prevNode
    newMetadata <- buildNewMetadata node
    unless (or $ skipsDbUpdate <$> prevMetaData <*> Just newMetadata) do
        updateDb context node prevNode newMetadata status
    modifyIORef context.dbCache $ HM.delete node
    return (prevMetaData, newMetadata)

getNodeInfoCached :: DeciderContext -> Node -> IO (Maybe Nodes)
getNodeInfoCached context node = do
    cached <- HM.lookup node <$> readIORef context.dbCache
    case cached of
        Just ni -> return ni
        Nothing -> do
            let (dbtype, name) = dbName node
            ni <- getNodeInfo context.conn dbtype name
            updateNodeInfoCache context node ni
            return ni

updateNodeInfoCache :: DeciderContext -> Node -> Maybe Nodes -> IO ()
updateNodeInfoCache context node ni = modifyIORef context.dbCache $ HM.insert node ni

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
buildNewMetadata (ValueNode _ value _) = return $ ValMetaData $ hash $ BE.encode BE.utf8 $ T.pack value

updateDb :: DeciderContext -> Node -> Maybe Nodes -> MetaData -> Maybe Bool-> IO ()
updateDb context node prevNode newMetadata status = do
    ni <- case prevNode of
        Nothing -> initNodeInfo   context.conn dbtype name (dbExists newMetadata) (dbTimestamp newMetadata) (dbSignature newMetadata) Nothing status
                    where (dbtype, name) = dbName node
        Just ni -> updateNodeInfo context.conn ni          (dbExists newMetadata) (dbTimestamp newMetadata) (dbSignature newMetadata) Nothing (status `mplus` ni.task_status)
    updateNodeInfoCache context node $ Just ni

toPosix path = case coercionToPlatformTypes of
    Right (_, coercion) -> coerceWith coercion path

gainTimestamp :: IO Int64
gainTimestamp = do
    t <- getPOSIXTime
    return $ mkTimestamp t

mkTimestamp t = floor $ ((nominalDiffTimeToSeconds t) * 1e9)
