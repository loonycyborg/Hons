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
import Data.ByteString.Short (fromShort)
import qualified Data.HashMap.Strict as HM
import System.OsPath
import System.OsString ( coercionToPlatformTypes )
import System.File.OsPath ( readFile' )
import System.Posix.Files.PosixString
import System.OsString.Internal.Types (PosixString(getPosixString))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX
import qualified Crypto.Hash.MD5 as MD5
import Data.IORef
import System.IO.Unsafe

import Node
import Db
import Action (EvalResult (EvalResult), noResult)
import Value

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
    TaskMetaData { status :: Bool, task_signature :: Maybe B.ByteString }
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
dbName (ValueNode name) = (T.pack "value", T.pack name)

fromDb :: Nodes -> MetaData
fromDb (Nodes _ _ _ nodeType name existed timestamp signature _ _)
    | nodeType == T.pack "value" = ValMetaData signature
    | not existed                = Nonexistent
    | otherwise                  = MetaData timestamp signature

fromDbTask :: Nodes -> Maybe TaskMetaData
fromDbTask (Nodes _ _ _ _ _ _ _ _ task_signature task_status) = TaskMetaData <$> task_status <*> Just task_signature

data Ruling = Unchanged | Changed deriving (Show, Eq)

instance Semigroup Ruling where
    Changed   <> _ = Changed
    Unchanged <> x = x

instance Monoid Ruling where
    mempty = Unchanged

decideNode :: DeciderContext -> Node -> EvalResult -> IO Ruling
decideNode context node result = do
    (prevMetaData, newMetadata) <- syncDb context node Nothing result
    return $ fromMaybe Changed $ nodeChanged <$> prevMetaData <*> Just newMetadata

needsRebuild :: DeciderContext -> Node -> [B.ByteString] -> IO Bool
needsRebuild decider node@(FsNode path) task_signature = do
    exists <- fileExist $ toPosix path
    prevNode <- getNodeInfoCached decider node
    let prevResult    = fromMaybe False $ (.task_status)    =<< prevNode
    let prevSignature =                   (.task_signature) =<< prevNode
    let signature     = hashSignature task_signature
    return $ not prevResult || not exists || signature /= prevSignature
needsRebuild decider (ValueNode {}) _ = return False

wasRebuilt :: DeciderContext -> Node -> Bool -> [B.ByteString] -> IO ()
wasRebuilt context node status signature = do
    void $ syncDb context node (Just $ TaskMetaData status $ hashSignature signature) noResult

syncDb :: DeciderContext -> Node -> Maybe TaskMetaData -> EvalResult -> IO (Maybe MetaData, MetaData)
syncDb context node task_metadata result = do
    prevNode <- getNodeInfoCached context node
    let prevMetaData = fromDb <$> prevNode
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

buildNewMetadata :: Node -> EvalResult -> IO MetaData
buildNewMetadata (FsNode path) _ = do
    exists <- fileExist $ toPosix path
    case exists of
        True -> do
            fStatus <- getFileStatus $ toPosix path
            signature <- unsafeInterleaveIO do
                MD5.hash <$> readFile' path
            return $ MetaData (mkTimestamp $ modificationTimeHiRes fStatus) signature
        False -> return Nonexistent
buildNewMetadata (ValueNode _) result = return $ ValMetaData $ hashResult result

updateDb :: DeciderContext -> Node -> Maybe Nodes -> MetaData -> Maybe TaskMetaData -> IO ()
updateDb context node prevNode newMetadata newTaskMetadata = do
    let (task_signature, status) = case newTaskMetadata of
            Just (TaskMetaData s t) -> (t, Just s)
            Nothing -> (Nothing, Nothing)
    ni <- case prevNode of
        Nothing -> initNodeInfo   context.conn dbtype name (dbExists newMetadata) (dbTimestamp newMetadata) (dbSignature newMetadata) task_signature status
                    where (dbtype, name) = dbName node
        Just ni -> updateNodeInfo context.conn ni          (dbExists newMetadata) (dbTimestamp newMetadata) (dbSignature newMetadata) task_signature status
    updateNodeInfoCache context node $ Just ni

hashSignature :: [B.ByteString] -> Maybe B.ByteString
hashSignature []    = Nothing
hashSignature parts = Just $ MD5.finalize ctx where
    ctx  = foldl MD5.update ctx0 parts
    ctx0 = MD5.init

hashResult :: EvalResult -> B.ByteString
hashResult (EvalResult result) = fromJust $ hashSignature $ fromShort . getPosixString . toPosix <$> toCmdLine result

toPosix path = case coercionToPlatformTypes of
    Right (_, coercion) -> coerceWith coercion path

gainTimestamp :: IO Int64
gainTimestamp = mkTimestamp <$> getPOSIXTime

mkTimestamp t = floor $ nominalDiffTimeToSeconds t * 1e9
