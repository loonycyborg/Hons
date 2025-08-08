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
import System.Posix.Files.PosixString
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX
import System.IO.Unsafe (unsafePerformIO)

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
    do fmap DeciderContext $ openDb dbFile
    do close . (.conn)

dbTypeName :: Node -> T.Text
dbTypeName (FsNode {})    = T.pack "fs"
dbTypeName (ValueNode {}) = T.pack "value"

data Ruling = Unchanged | Changed | Undecided deriving (Show, Eq, Enum)

instance Semigroup Ruling where
    x <> y = if fromEnum x > fromEnum y then x else y

instance Monoid Ruling where
    mempty = Unchanged

decideNode :: DeciderContext -> Node -> IO Ruling
decideNode context node = do
    name <- case node of 
                ValueNode name _ _ -> return $ T.pack name
                FsNode path        -> fmap T.pack $ decodeFS path
    prevNode <- getNodeInfo context.conn (dbTypeName node) name
    let prevMetaData = case prevNode of
            Just (Nodes _ _ _ nodeType name existed timestamp signature _ _) ->
                if existed then
                    if nodeType == T.pack "value" then
                        ValMetaData signature
                    else
                        MetaData timestamp signature
                else Nonexistent
            Nothing -> Nonexistent
    newMetadata <- buildNewMetadata node
    let changed = case isNothing prevNode || (prevMetaData /= newMetadata) of
            True  -> Changed
            False -> Unchanged
    when (changed == Changed) do
        case prevNode of
            Nothing -> initNodeInfo   context.conn (dbTypeName node) name (dbExists newMetadata) (dbTimestamp newMetadata) (dbSignature newMetadata) Nothing Nothing
            Just ni -> updateNodeInfo context.conn ni                     (dbExists newMetadata) (dbTimestamp newMetadata) (dbSignature newMetadata) Nothing Nothing
    return changed

decideNodePure :: DeciderContext -> Node -> Ruling
decideNodePure decider node = unsafePerformIO $ decideNode decider node

buildNewMetadata :: Node -> IO MetaData
buildNewMetadata (FsNode path) = do
    exists <- fileExist $ toPosix path
    if exists then do
        fStatus <- getFileStatus $ toPosix path
        return $ MetaData (mkTimestamp $ modificationTimeHiRes fStatus) B.empty
    else
        return Nonexistent
buildNewMetadata (ValueNode {}) = return $ ValMetaData B.empty

toPosix path = case coercionToPlatformTypes of
    Right (_, coercion) -> coerceWith coercion path

gainTimestamp :: IO Int64
gainTimestamp = do
    t <- getPOSIXTime
    return $ mkTimestamp t

mkTimestamp t = floor $ ((nominalDiffTimeToSeconds t) * 1e9)
