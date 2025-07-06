{-# LANGUAGE BlockArguments, GADTs, QuasiQuotes #-}
import GHC
import GHC.Data.StringBuffer
import GHC.Paths ( libdir )
import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )
import GHC.Driver.DynFlags
import GHC.Data.EnumSet
import GHC.LanguageExtensions.Type
import GHC.Version
import Distribution.Client.Config
import Control.Monad.IO.Class
import Data.Time.Clock
import Type.Reflection
import Unsafe.Coerce
import System.Directory.OsPath
import System.OsPath
import System.IO.Unsafe (unsafePerformIO)

import Algebra.Graph.Export.Dot (exportViaShow)

import Project
import Node
import DepGraph
import Taskmaster

inplaceDbPath userStore exe = dbPath $ break (==[osp|dist-newstyle|]) $ splitDirectories exe
  where
  dbPath (_,[]) = []
  dbPath (ps, _) = fmap decode
    [ foldr1 (</>) ps <> [osp|/dist-newstyle/packagedb/ghc-|] <> ghcVersionSuffix
    , encode userStore </> [osp|ghc-|] <> ghcVersionSuffix <> [osp|-inplace/package.db|]
    ]
    where
    decode = unsafePerformIO . decodeFS
    encode = unsafePerformIO . encodeFS
    ghcVersionSuffix = unsafePerformIO $ encodeFS cProjectVersion

pPrint :: (Show a, MonadIO m) => a -> m ()
pPrint = liftIO . print
hons_prelude = stringToStringBuffer "module Honstruct (project) where\nimport Project\n{-# LINE 1 \"Honstruct\" #-}\n"
hons_epilogue = stringToStringBuffer "\nproject :: Project"
main = defaultErrorHandler defaultFatalMessager defaultFlushOut do
    runGhc (Just libdir) do
        dflags <- getSessionDynFlags
        userStore <- liftIO defaultStoreDir
        dbPath <- fmap (inplaceDbPath userStore) $ liftIO $ getSymbolicLinkTarget [osp|/proc/self/exe|]
        setSessionDynFlags dflags
            { backend   = interpreterBackend
            , ghcLink   = LinkInMemory
            , extensionFlags = dflags.extensionFlags <> fromList [ OverloadedRecordDot, QuasiQuotes, DataKinds, BlockArguments ] `difference` (fromList [FieldSelectors])
            , packageDBFlags = map (PackageDB . PkgDbPath) dbPath }
        let script_file = "Honstruct"
        src <- liftIO do
            src <- hGetStringBuffer script_file
            src <- appendStringBuffers hons_prelude src
            src <- appendStringBuffers src hons_epilogue
            t <- getCurrentTime
            return $ Just (src,t)
        let target = Target (TargetFile script_file Nothing) True dflags.homeUnitId_ src
        setTargets [target]
        load LoadAllTargets
        setContext [ IIModule $ mkModuleName "Honstruct" ]
        v <- compileExpr "project"
        liftIO $ do_build $ unsafeCoerce v

do_build (Project e r) = do
    let g = r.graph
    let t = r.tasks
    writeFile "graph.dot" (exportViaShow g)
    print t
    let order = buildOrder r [fs|example/hello|]
    print order
    result <- build g e order
    print result