{-# LANGUAGE BlockArguments
           , DerivingStrategies
           , DeriveGeneric
           , DeriveAnyClass
           , UndecidableInstances
           , DataKinds
           , EmptyDataDecls
           , OverloadedStrings
           , QuasiQuotes
           , TemplateHaskellQuotes
           , TypeFamilies
#-}
module Db where
import qualified Data.ByteString as B
import Control.Monad.IO.Class  (liftIO)
import Database.Beam.Sqlite
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite.Migrate
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Language.Haskell.TH (Extension(DeriveAnyClass))
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import System.Directory (removeFile, doesFileExist)
import Control.Monad (when)
import System.Posix (modificationTimeHiRes, getFileStatus)

data NodesT f
    = Nodes
    { nodeId         :: Columnar f Int32
    , persistent_id  :: Columnar f Int64
    , generation     :: Columnar f Int64
    , nodeType       :: Columnar f T.Text
    , name           :: Columnar f T.Text
    , existed        :: Columnar f Bool
    , timestamp      :: Columnar f Int64
    , signature      :: Columnar f B.ByteString
    , task_signature :: Columnar f (Maybe B.ByteString)
    , task_status    :: Columnar f (Maybe Bool) }
    deriving (Generic, Beamable)

instance Table NodesT where
    data PrimaryKey NodesT f = NodeId (Columnar f Int32) deriving (Generic, Beamable)
    primaryKey = NodeId <$> (.nodeId)

type Nodes = NodesT Identity
deriving instance Show Nodes
deriving instance Eq Nodes

data NodeMetaData f = NodeMetaData
    { nodes :: f (TableEntity NodesT) }
      deriving (Generic, Database Sqlite)

nodeMetaDataChecked :: CheckedDatabaseSettings Sqlite NodeMetaData
nodeMetaDataChecked = defaultMigratableDbSettings

nodeMetaData :: DatabaseSettings Sqlite NodeMetaData
nodeMetaData = unCheckDatabase nodeMetaDataChecked

setupSchema conn = do
    execute_ conn [sql|
        create table if not exists nodes(
            id INTEGER PRIMARY KEY NOT NULL,
            persistent_id BIGINT NOT NULL,
            generation BIGINT NOT NULL,
            type VARCHAR NOT NULL,
            name VARCHAR NOT NULL,
            existed BOOLEAN NOT NULL,
            timestamp BIGINT NOT NULL,
            signature BLOB NOT NULL,
            task_signature BLOB,
            task_status BOOL
        )
    |]

    execute_ conn "create index if not exists node_identity_index on nodes (type, name)"
    execute_ conn "create unique index if not exists node_archive_index on nodes (generation, type, name)"
    execute_ conn "create index if not exists node_persistent_index on nodes (persistent_id)"

    runBeamSqlite conn do
        result <- verifySchema migrationBackend nodeMetaDataChecked
        case result of
            VerificationSucceeded -> return conn
            VerificationFailed predicates -> do
                liftIO $ mapM print predicates
                fail "Beam and SQL schemes don't match!"

openDb filename = do
    existed <- doesFileExist filename
    conn <- open filename
    runBeamSqlite conn do
        result <- verifySchema migrationBackend nodeMetaDataChecked
        case result of
            VerificationSucceeded -> return conn
            _ -> liftIO do
                when existed do
                    putStrLn "Failed to recognize node database scheme. It will be reinitialized."
                close conn
                removeFile filename
                conn <- open filename
                setupSchema conn

getNodeInfo :: Connection -> T.Text -> T.Text -> IO (Maybe Nodes)
getNodeInfo conn nodeType name = do
    r <- runBeamSqlite conn do
        runSelectReturningList $
            select do
                (_, _, generation) <- filter_ (\(t, n, _) -> val_ name ==. n &&. val_ nodeType ==. t) $
                    aggregate_ (\node -> (group_ node.nodeType, group_ node.name, max_ node.generation)) $ all_ nodeMetaData.nodes
                filter_ (\node -> node.nodeType ==. val_ nodeType &&. node.name ==. val_ name &&. maybe_ (val_ False) (\g -> node.generation ==. g) generation) $ all_ nodeMetaData.nodes
    return case r of
        [n] -> Just n
        []   -> Nothing

initNodeInfo :: Connection -> T.Text -> T.Text -> Bool -> Int64 -> B.ByteString -> Maybe B.ByteString -> Maybe Bool -> IO Nodes
initNodeInfo conn nodeType name exists timestamp signature taskSignature taskStatus = do
    runBeamSqlite conn do
        Just next_persistent_id <- runSelectReturningOne $ select do
            aggregate_ (\node -> maybe_ 0 (+1) (max_ node.persistent_id)) $ all_ nodeMetaData.nodes
        [result] <- runInsertReturningList do
            insertReturning nodeMetaData.nodes $
                insertExpressions [Nodes
                    default_
                    (val_ next_persistent_id)
                    0
                    (val_ nodeType)
                    (val_ name)
                    (val_ exists)
                    (val_ timestamp)
                    (val_ signature)
                    (val_ taskSignature)
                    (val_ taskStatus)
                ]
        return result

updateNodeInfo :: Connection -> Nodes -> Bool -> Int64 -> B.ByteString -> Maybe B.ByteString -> Maybe Bool -> IO Nodes
updateNodeInfo conn prevNodeInfo exists timestamp signature taskSignature taskStatus = do
    runBeamSqlite conn do
        [result] <- runInsertReturningList do
            insertReturning nodeMetaData.nodes $
                insertExpressions [Nodes 
                    default_
                    (val_ prevNodeInfo.persistent_id)
                    (val_ prevNodeInfo.generation + 1)
                    (val_ prevNodeInfo.nodeType)
                    (val_ prevNodeInfo.name)
                    (val_ exists)
                    (val_ timestamp)
                    (val_ signature)
                    (val_ taskSignature)
                    (val_ taskStatus)
                ]
        return result
