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
import Data.ByteString (ByteString, toStrict)
import Data.IORef
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Arrow
import Data.Bifunctor ( Bifunctor(bimap) )

import Action
import Node
import Environment
import Decider
import DepGraph
import System.IO.Unsafe (unsafePerformIO)
import Data.Binary (encode)

data TaskmasterSettings = TaskmasterSettings {
    jobs :: Int,
    alwaysMake :: Bool,
    keepGoing :: Bool
} deriving (Eq, Show)

data TaskStatus vars where
    Done    :: { target :: Node, env :: Environment vars, implicit :: [(Node, Node)], changed :: Ruling } -> TaskStatus vars
    Pending :: { target :: Node, async :: Maybe (Async (TaskStatus vars)) } -> TaskStatus vars
    Failed  :: { target :: Node } -> TaskStatus vars
    deriving Show

instance Show (Async (TaskStatus vars)) where
    show _ = "<Pending Async>"

taskFailed :: TaskStatus vars -> Bool
taskFailed (Failed {}) = True
taskFailed _           = False

pendingAsync :: TaskStatus vars -> Maybe (Async (TaskStatus vars))
pendingAsync (Pending _ (Just as)) = Just as
pendingAsync _                     = Nothing

data BuildException = TaskFailed deriving (Show)
instance Exception BuildException

classifyStatuses :: Foldable t => t (TaskStatus vars) -> ([TaskStatus vars], [TaskStatus vars], [TaskStatus vars])
classifyStatuses = foldr classifyStatus ([], [], []) where
    classifyStatus c (lDone, lPending, lFailed) =
        case c of
            Done {}    -> (c:lDone,   lPending,   lFailed)
            Pending {} -> (  lDone, c:lPending,   lFailed)
            Failed {}  -> (  lDone,   lPending, c:lFailed)

executeTask :: Typeable vars => Environment vars -> Task vars -> IO (Bool, Environment vars)
executeTask env task@(Task targets sources action sign) = do
    runReaderT (runStateT action env) task

signTask :: Environment vars -> Task vars -> IO [ByteString]
signTask env task =
    fst <$> runReaderT (runStateT task.sign env) task

executeEvaluator :: Environment vars -> Task vars -> IO ((EvalResult, [(Node, Node)]), Environment vars)
executeEvaluator env task@(Evaluator target _ eval _) = let ?target = target in do
    catch
        do runReaderT (runStateT eval env) task
        do \(e :: SomeException) -> putStrLn ("hons: " <> show target <> " : evaluation threw exception: " <> displayException e) >> return ((ResultFailure, []), env)

build :: Typeable vars => TaskmasterSettings -> RuleSet vars -> Environment vars -> Node -> IO (Bool, DepGraph)
build settings ruleset env goal = withDeciderContext "honsign.sqlite" \decider -> do
    task_cache <- newIORef HM.empty
    parallel_limit <- case settings.jobs of
        0 -> return Nothing
        _ -> Just <$> newQSem settings.jobs
    let
        extract_implicit a = (a, case a of
            Done _ _ implicit _ -> implicit
            _ -> []
            )
        buildNode _         xs       _     _     ((b:_):bs) = error $ "Dependency cycle detected: " ++ show (b : reverse (b : takeWhile (/=b) xs))
        buildNode prev_pass (node:_) tsrcs osrcs []         = extract_implicit . unsafePerformIO $ do
            let allsrcs = tsrcs <> osrcs
            let (done, pending, failed) = classifyStatuses allsrcs
            case HM.lookup node prev_pass of
                Nothing                  -> evaluateNode pending done failed
                Just (Pending _ Nothing) -> evaluateNode pending done failed
                Just p@(Pending _ (Just as)) -> do
                    r <- poll as
                    case r of
                        Just (Right status) -> return status
                        Nothing -> return p
                Just status -> return status
            where
                task = node `HM.lookup` ruleset.tasks
                evaluateNode _      _   (_:_) = return $ Failed node
                evaluateNode (_:_)  _      [] = return $ Pending node Nothing
                evaluateNode []     done   [] = do
                    let source_env = if null done then env else foldr1 eMerge $ map (.env) done
                    let implicit_deps = map ((.target) &&& (.implicit)) $
                            filter (not . null . (.implicit)) $
                            filter ((/=Unchanged) . (.changed)) done
                    updateImplicitDeps decider implicit_deps
                    case task of
                        Nothing -> returnUpToDate source_env []
                        Just t  -> do
                            let sources_changed = mconcat $ map (.changed) done
                            signature <- signTask source_env t
                            needs_rebuild <- if null t.sources then return True else needsRebuild decider node signature
                            case (sources_changed, needs_rebuild || settings.alwaysMake) of
                                (Unchanged, False) -> do
                                    implicit <- reuseImplicitDeps decider node
                                    returnUpToDate source_env implicit
                                _                  -> Pending node . Just <$> async do
                                    new_var <- newEmptyMVar
                                    cached_var <- atomicModifyIORef' task_cache \cache ->
                                        case HM.lookup t cache of
                                            Just st -> (cache, st)
                                            Nothing -> (HM.insert t new_var cache, new_var)
                                    var <- if new_var == cached_var then do
                                        status <- parallel_limiter do
                                            case t of
                                                Task {}      -> do
                                                    (result, result_env) <- executeTask source_env t
                                                    changed <- wasRebuilt decider node result Nothing signature
                                                    if result then
                                                        return $ Done node result_env [] changed
                                                    else
                                                        returnFail
                                                Evaluator {} -> do
                                                    ((result, implicit), result_env) <- executeEvaluator source_env t
                                                    let (is_success, value) = case result of
                                                            ResultFailure -> (False, Nothing)
                                                            EvalResult a -> (True, Just $ toStrict $ encode a)
                                                    changed <- wasRebuilt decider node is_success value signature
                                                    case result of
                                                        ResultFailure -> returnFail
                                                        _             -> return $ Done node result_env implicit changed
                                        when (taskFailed status) do
                                            putStrLn $ "hons: *** " ++ show t ++ ": task failed"
                                            unless settings.keepGoing do
                                                throwIO TaskFailed
                                        putMVar new_var status
                                        return new_var
                                    else
                                        return cached_var
                                    readMVar var
                returnFail = return $ Failed node
                returnUpToDate env implicit = do
                    changed <- decideNode decider node Nothing
                    return $ Done node env implicit changed
        parallel_limiter =
            case parallel_limit of
                Just sem -> bracket_ (waitQSem sem) (signalQSem sem)
                Nothing  -> id
    result <- bimap (not . taskFailed) ((ruleset.graph <>) . implicit_edges) <$> catch
        do
            let build_graph prev = do
                    r@(a, st) <- evaluate $ depthFirstFold (flip (:)) (buildNode prev) ruleset.graph goal []
                    case a of
                        Pending {} -> do
                            waitAny $ mapMaybe pendingAsync (HM.elems st)
                            build_graph st
                        _ -> return r
                in build_graph HM.empty
        do \(e :: BuildException) -> return (Failed goal, HM.empty)
    unless (fst result) do
        putStrLn "hons: *** build failed"
    return result
    where implicit_edges statuses = overlays $ implicit_node_edges <$> adjacencyList ruleset.graph where
            implicit_node_edges (source, targets) = edges $ targets >>= (maybe [] (.implicit) <$> (`HM.lookup` statuses))
