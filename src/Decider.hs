{-# LANGUAGE BlockArguments, OverloadedStrings #-}
module Decider where

import Data.Int (Int32, Int64)
import Data.Maybe
import Control.Monad
import Control.Exception
import Data.Bifunctor (bimap)
import Database.SQLite.Simple
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.ByteString.Short (fromShort)
import qualified Data.HashMap.Strict as HM
import System.OsPath
import System.File.OsPath ( readFile' )
import System.Directory.OsPath (doesFileExist)
import qualified Crypto.Hash.MD5 as MD5
import Data.IORef
import System.IO.Unsafe

import Node
import Db
import Action (EvalResult (EvalResult), noResult)
import Value
import FileCompat

data DeciderContext = DeciderContext {
    dbCache :: IORef (HM.HashMap Node (Maybe Nodes)),
    conn :: Connection
}

data MetaData where
    MetaData    :: { timestamp :: Int64, signature :: B.ByteString } -> MetaData
    ValMetaData :: { signature :: B.ByteString } -> MetaData
    Nonexistent :: {} -> MetaData
    deriving (Eq, Show)

data TaskMetaData =
    TaskMetaData { status :: Bool, result_value :: Maybe B.ByteString, task_signature :: Maybe B.ByteString }
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
        cache <- newIORef HM.empty
        DeciderContext cache <$> openDb dbFile
    do close . (.conn)

dbName :: Node -> (T.Text, T.Text)
dbName (FsNode path) = unsafePerformIO do
                         p <- decodeFS path
                         pure (T.pack "fs",    T.pack p)
dbName (ValueNode name (ValueTyHolder t)) = (T.pack "value", T.pack name <> T.pack (valueTypeSuffix t))

fromDbName :: (T.Text, T.Text) -> Node
fromDbName ("fs", name) = mkFsNodeFromString $ T.unpack name
fromDbName ("value", name) = mkValue $ T.unpack name

fromDb :: Nodes -> MetaData
fromDb (Nodes _ _ _ nodeType name existed value timestamp signature _ _)
    | nodeType == T.pack "value" = ValMetaData signature
    | not existed                = Nonexistent
    | otherwise                  = MetaData timestamp signature

fromDbTask :: Nodes -> Maybe TaskMetaData
fromDbTask (Nodes _ _ _ _ _ _ result _ _ task_signature task_status) = TaskMetaData <$> task_status <*> Just result <*> Just task_signature

data Ruling = Unchanged | Changed deriving (Show, Eq)

instance Semigroup Ruling where
    Changed   <> _ = Changed
    Unchanged <> x = x

instance Monoid Ruling where
    mempty = Unchanged

decideNode :: DeciderContext -> Node -> Maybe TaskMetaData -> IO Ruling
decideNode context node result = do
    (prevMetaData, newMetadata) <- syncDb context node result
    return $ fromMaybe Changed $ nodeChanged <$> prevMetaData <*> Just newMetadata

needsRebuild :: DeciderContext -> Node -> [B.ByteString] -> IO Bool
needsRebuild decider node task_signature = do
    exists <- case node of
        FsNode path -> doesFileExist path
        _           -> return True
    prevNode <- getNodeInfoCached decider node
    let prevResult    = fromMaybe False $ (.task_status)    =<< prevNode
    let prevSignature =                   (.task_signature) =<< prevNode
    let signature     = hashSignature task_signature
    return $ not prevResult || not exists || signature /= prevSignature

wasRebuilt :: DeciderContext -> Node -> Bool -> Maybe B.ByteString -> [B.ByteString] -> IO Ruling
wasRebuilt context node status result signature = do
    decideNode context node (Just $ TaskMetaData status result $ hashSignature signature)

syncDb :: DeciderContext -> Node -> Maybe TaskMetaData -> IO (Maybe MetaData, MetaData)
syncDb context node task_metadata = do
    prevNode <- getNodeInfoCached context node
    let prevMetaData = fromDb <$> prevNode
    let result = ((.result_value)) =<< task_metadata
    newMetadata <- buildNewMetadata node result
    let prev_task_metadata = fromDbTask <$> prevNode
    let skip_update = or $ skipsDbUpdate <$> prevMetaData <*> Just newMetadata
    let skip_task_update = isNothing task_metadata || Just task_metadata == prev_task_metadata
    unless (skip_update && skip_task_update) do
        updateDb context node prevNode newMetadata task_metadata
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

buildNewMetadata :: Node -> Maybe B.ByteString -> IO MetaData
buildNewMetadata (FsNode path) _ = do
    exists <- doesFileExist path
    case exists of
        True -> do
            timestamp <- timestampFile path
            signature <- unsafeInterleaveIO do
                MD5.hash <$> readFile' path
            return $ MetaData timestamp signature
        False -> return Nonexistent
buildNewMetadata (ValueNode _ _) result = return $ ValMetaData $ fromMaybe B.empty result

updateDb :: DeciderContext -> Node -> Maybe Nodes -> MetaData -> Maybe TaskMetaData -> IO ()
updateDb context node prevNode newMetadata newTaskMetadata = do
    let (task_signature, status, result) = case newTaskMetadata of
            Just (TaskMetaData s r t) -> (t, Just s, r)
            Nothing -> (Nothing, Nothing, Nothing)
    ni <- case prevNode of
        Nothing -> initNodeInfo   context.conn dbtype name (dbExists newMetadata) result (dbTimestamp newMetadata) (dbSignature newMetadata) task_signature status
                    where (dbtype, name) = dbName node
        Just ni -> updateNodeInfo context.conn ni          (dbExists newMetadata) result (dbTimestamp newMetadata) (dbSignature newMetadata) task_signature status
    updateNodeInfoCache context node $ Just ni

updateImplicitDeps :: DeciderContext -> [(Node, [Node])] -> IO()
updateImplicitDeps decider imps = do
    cache <- readIORef decider.dbCache
    let ids = map (bimap lookup_item (map lookup_item)) imps
        lookup_item = (.nodeId) . fromMaybe (error msg) . join . flip HM.lookup cache
        msg = "Failed to find node in database"
    insertImplicitDeps decider.conn ids

reuseImplicitDeps :: DeciderContext -> Node -> IO [Node]
reuseImplicitDeps decider node = do
    Just ni <- getNodeInfoCached decider node
    map fromDbName <$> selectImplicitDeps decider.conn ni.nodeId

hashSignature :: [B.ByteString] -> Maybe B.ByteString
hashSignature []    = Nothing
hashSignature parts = Just $ MD5.finalize ctx where
    ctx  = foldl MD5.update ctx0 parts
    ctx0 = MD5.init

hashResult :: EvalResult -> B.ByteString
hashResult (EvalResult result) = fromMaybe B.empty $ hashSignature $ toSignature result
