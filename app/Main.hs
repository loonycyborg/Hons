{-# LANGUAGE BlockArguments, GADTs #-}
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
import Data.Bool (bool)
import System.Exit (exitFailure, exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import Options.Applicative (execParser)

import Algebra.Graph.Export.Dot (exportViaShow)

import Project
import Node
import DepGraph
import Taskmaster
import Builder (depends)

import Options

envFName = ".ghc.environment." <> intercalate "-" [arch, os, cProjectVersion]
    where
        arch = stringEncodeArch hostPlatformArch
        os = stringEncodeOS hostPlatformOS

findEnv :: MonadIO m => m (Maybe FilePath)
findEnv = liftIO do
    file <- join <$> sequence executablePath
    return do
        exe <- file
        case break (=="dist-newstyle") $ splitDirectories exe of
            (path, _:_) -> Just $ foldr1 (</>) path </> envFName
            (_, []) -> Nothing

pPrint :: (Show a, MonadIO m) => a -> m ()
pPrint = liftIO . print
honsPrelude = stringToStringBuffer "module Honstruct (project) where\nimport Hons\nimport qualified Tool.CC as CC\n{-# LINE 1 \"Honstruct\" #-}\n"
honsEpilogue = stringToStringBuffer "\nproject :: Project\nproject = Project (makeEnv env) rules (toList defaultTargets)"
main = defaultErrorHandler defaultFatalMessager defaultFlushOut do
    (invoc_settings, taskmaster_settings) <- execParser opts
    runGhc (Just libdir) do
        logger <- getLogger
        env <- findEnv
        dflags <- getSessionDynFlags
        let act = maybe return (const $ interpretPackageEnv logger) env
        dflags <- liftIO $ act dflags
            { backend   = interpreterBackend
            , ghcLink   = LinkInMemory
            , extensionFlags = dflags.extensionFlags <> fromList [ OverloadedRecordDot, QuasiQuotes, DataKinds, BlockArguments ] `difference` fromList [FieldSelectors]
            , packageEnv = env }
        setSessionDynFlags dflags
        let script_file = invoc_settings.file
        src <- liftIO do
            mapM_ setCurrentDirectory invoc_settings.chdir
            src <- hGetStringBuffer script_file >>=
                   appendStringBuffers honsPrelude >>=
                   flip appendStringBuffers honsEpilogue
            t <- getCurrentTime
            return $ Just (src,t)
        let target = Target (TargetFile script_file Nothing) True dflags.homeUnitId_ src
        setTargets [target]
        load LoadAllTargets
        setContext [ IIModule $ mkModuleName "Honstruct" ]
        v <- compileExpr "project"
        liftIO $ doBuild taskmaster_settings invoc_settings.cmdlineTargets $ unsafeCoerce v

doBuild settings target_strings (Project e r default_targets) = do
    print settings
    let g = r.graph
    let t = r.tasks
    writeFile "graph.dot" (exportViaShow g)
    print t
    let targets = if null target_strings then
            default_targets
        else
            map (resolveTarget g) target_strings
    let goal_graph = r <> depends goal targets
    result <- build settings goal_graph e goal
    when (null targets) do
        print "hons: warning: no targets built because no targets in command line and no default targets in build script"
    bool exitFailure exitSuccess result
