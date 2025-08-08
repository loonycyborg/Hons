{-# LANGUAGE BlockArguments, GADTs, QuasiQuotes #-}
import GHC
import GHC.Data.StringBuffer
import GHC.Paths ( libdir )
import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )
import GHC.Driver.DynFlags
import GHC.Data.EnumSet
import GHC.LanguageExtensions.Type
import GHC.Version
import GHC.Platform.Host
import GHC.Platform.ArchOS
import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import Type.Reflection
import Unsafe.Coerce
import System.Directory
import System.FilePath
import System.Environment
import Data.List
import System.IO.Unsafe (unsafePerformIO)

import Algebra.Graph.Export.Dot (exportViaShow)

import Project
import Node
import DepGraph
import Taskmaster

envFName = ".ghc.environment." <> intercalate "-" [arch, os, cProjectVersion]
    where
        arch = stringEncodeArch hostPlatformArch
        os = stringEncodeOS hostPlatformOS

findEnv :: MonadIO m => m (Maybe FilePath)
findEnv = liftIO do
    file <- fmap join $ sequence executablePath
    return do
        exe <- file
        case break (=="dist-newstyle") $ splitDirectories exe of
            (path, _:_) -> Just $ (foldr1 (</>) path) </> envFName
            (_, []) -> Nothing

pPrint :: (Show a, MonadIO m) => a -> m ()
pPrint = liftIO . print
hons_prelude = stringToStringBuffer "module Honstruct (project) where\nimport Hons\nimport qualified Tool.CC as CC\n{-# LINE 1 \"Honstruct\" #-}\n"
hons_epilogue = stringToStringBuffer "\nproject :: Project\nproject = Project (makeEnv env) rules"
main = defaultErrorHandler defaultFatalMessager defaultFlushOut do
    runGhc (Just libdir) do
        logger <- getLogger
        env <- findEnv
        dflags <- getSessionDynFlags
        let act = maybe return (const $ interpretPackageEnv logger) env
        dflags <- liftIO $ act dflags
            { backend   = interpreterBackend
            , ghcLink   = LinkInMemory
            , extensionFlags = dflags.extensionFlags <> fromList [ OverloadedRecordDot, QuasiQuotes, DataKinds, BlockArguments ] `difference` (fromList [FieldSelectors])
            , packageEnv = env }
        setSessionDynFlags dflags
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
