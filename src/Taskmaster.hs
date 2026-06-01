{-# LANGUAGE OverloadedRecordDot, BlockArguments, ImplicitParams #-}
module Taskmaster where
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.List.NonEmpty as L
import Type.Reflection
import Algebra.Graph.AdjacencyMap
import Control.Monad
import Data.Either
import Data.Maybe
import Data.ByteString (ByteString)
import Data.IORef
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception

import Action
import Node
import Environment
import Decider
import DepGraph
import System.IO.Unsafe (unsafePerformIO)

data TaskmasterSettings = TaskmasterSettings {
    jobs :: Int,
    alwaysMake :: Bool,
    keepGoing :: Bool
} deriving (Eq, Show)

data TaskStatus vars where
    Done    :: { target :: Node, env :: Environment vars, implicit :: [Node], changed :: Ruling } -> TaskStatus vars
    Failed  :: { target :: Node } -> TaskStatus vars
    deriving Show

data BuildException = TaskFailed deriving (Show)
instance Exception BuildException

classifyStatuses :: Foldable t => t (TaskStatus vars) -> ([TaskStatus vars], [TaskStatus vars])
classifyStatuses = foldr classifyStatus ([], []) where
    classifyStatus c (lDone, lFailed) =
        case c of
            Done {}   -> (c:lDone,   lFailed)
            Failed {} -> (  lDone, c:lFailed)

executeTask :: Typeable vars => Environment vars -> Task vars -> IO (Bool, Environment vars)
executeTask env task@(Task targets sources action sign) = do
    runReaderT (runStateT action env) task

signTask :: Environment vars -> Task vars -> IO [ByteString]
signTask env task@(Task targets sources action sign) =
    fst <$> runReaderT (runStateT sign env) task

executeEvaluator :: Environment vars -> Task vars -> IO ((EvalResult, [Node]), Environment vars)
executeEvaluator env task@(Propagator target eval) = let ?target = target in do
    runReaderT (runStateT eval env) task

build :: Typeable vars => TaskmasterSettings -> RuleSet vars -> Environment vars -> Node -> IO Bool
build settings ruleset env goal = withDeciderContext "honsign.sqlite" \decider -> do
    task_cache <- newIORef HM.empty
    parallel_limit <- case settings.jobs of
        0 -> return Nothing
        _ -> Just <$> newQSem settings.jobs
    let
        extract_implicit a = (a, (unsafePerformIO do either wait return a).implicit)
        buildNode xs       _     _     ((b:_):bs) = error $ "Dependency cycle detected: " ++ show (b : reverse (b : takeWhile (/=b) xs))
        buildNode (node:_) tsrcs osrcs []         = extract_implicit . unsafePerformIO $ do
            let allsrcs = tsrcs <> osrcs
            let (pending, complete) = partitionEithers allsrcs
            let (done, failed) = classifyStatuses complete
            evaluateNode allsrcs pending done failed
            where
                task = node `HM.lookup` ruleset.tasks
                evaluateNode allsrcs (_:_) done _     = do
                    Left <$> async do
                        (async_done, async_failed) <- classifyStatuses <$> mapM (either wait return) allsrcs
                        evaluateNode allsrcs [] async_done async_failed >>= either wait return
                evaluateNode _       []    _    (_:_) = Right <$> returnFail
                evaluateNode _       []    done []    = do
                    let source_env = if null done then env else foldr1 eMerge $ map (.env) done
                    case task of
                        Nothing                   -> Right <$> returnSuccess source_env
                        Just eval@(Propagator {}) -> Right <$> (executeEvaluator source_env eval >>= returnSuccessEval)
                        Just t@(Task {})          -> do
                            let sources_changed = mconcat $ map (.changed) done
                            signature <- signTask source_env t
                            needs_rebuild <- needsRebuild decider node signature
                            case (sources_changed, needs_rebuild || settings.alwaysMake) of
                                (Unchanged, False) -> Right <$> returnSuccess source_env
                                _                  -> Left  <$> async do
                                    new_var <- newEmptyMVar
                                    cached_var <- atomicModifyIORef' task_cache \cache ->
                                        case HM.lookup t cache of
                                            Just st -> (cache, st)
                                            Nothing -> (HM.insert t new_var cache, new_var)
                                    var <- if new_var == cached_var then do
                                        (result, result_env) <- parallel_limiter do
                                            executeTask source_env t
                                        changed <- wasRebuilt decider node result signature
                                        unless result do
                                            putStrLn $ "hons: *** " ++ show t ++ ": task failed"
                                            unless settings.keepGoing do
                                                throwIO TaskFailed
                                        (if result then returnSuccessRebuilt result_env changed else returnFail) >>= putMVar new_var
                                        return new_var
                                    else
                                        return cached_var
                                    readMVar var
                returnFail = return $ Failed node
                returnSuccessRebuilt env changed             = returnSuccessG (\_ _ changed -> pure changed) env changed []
                returnSuccessEval ((result, implicit), env)  = returnSuccessG wasEvaluated                   env result  implicit
                returnSuccess env                            = returnSuccessG decideNode                     env Nothing []
                returnSuccessG f env arg implicit = do
                    changed <- f decider node arg
                    return $ Done node env implicit changed
        parallel_limiter =
            case parallel_limit of
                Just sem -> bracket_ (waitQSem sem) (signalQSem sem)
                Nothing  -> id
    result <- catch
        do either wait return $ depthFirstFold (flip (:)) buildNode ruleset.graph goal []
        do \(e :: BuildException) -> return $ Failed goal
    case result of
        Failed {} -> do
            putStrLn "hons: *** build failed"
            return False
        _         -> return True
