{-# LANGUAGE OverloadedRecordDot, BlockArguments #-}
module Taskmaster where
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.List.NonEmpty as L
import Type.Reflection
import Algebra.Graph.AdjacencyMap
import Control.Monad

import Action
import Node
import Environment
import Decider
import DepGraph
import Data.Maybe (mapMaybe, fromJust, isJust, isNothing)

data TaskStatus vars where
    Done    :: { target :: Node, env :: Environment vars, changed :: Ruling } -> TaskStatus vars
    Failed  :: { target :: Node } -> TaskStatus vars
    deriving Show

classifyStatuses :: Foldable t => t (TaskStatus vars) -> ([TaskStatus vars], [TaskStatus vars])
classifyStatuses = foldr classifyStatus ([], []) where
    classifyStatus c (lDone, lFailed) =
        case c of
            Done {}   -> (c:lDone,   lFailed)
            Failed {} -> (  lDone, c:lFailed)

executeTask :: Typeable vars => Environment vars -> Task vars -> IO (Bool, Environment vars)
executeTask env task@(Task targets sources action) =
    runReaderT (runStateT action env) task

transformWithNode :: Typeable vars => Node -> Environment vars -> Environment vars
transformWithNode (ValueNode _ _ tr) = eTransform tr
transformWithNode (FsNode _) = eTransform EIdentity

build :: Typeable vars => RuleSet vars -> Environment vars -> Node -> IO (TaskStatus vars)
build ruleset env goal = withDeciderContext "honsign.sqlite" \decider ->
        depthFirstFold (\l n -> n:l) (buildNode decider) ruleset.graph goal [] where
    buildNode decider xs       _    _ ((b:_):bs) = fail $ "Dependency cycle detected: " ++ (show $ b : (reverse $ b : takeWhile (/=b) xs))
    buildNode decider (node:_) srcs _ []         = do
        status <- case (srcs, node `HM.lookup` ruleset.tasks) of
            ([], task) -> do
                when (isJust task) do
                    fail $ "Invalid task without sources for target " ++ show node
                return $ Just (transformWithNode node env)
            (_, task) -> do
                (done, failed) <- fmap classifyStatuses $ sequence srcs
                if not $ null failed then
                    return Nothing
                else do
                    let source_env = foldr1 eMerge $ map (.env) done
                    case task of
                        Nothing -> return $ Just (transformWithNode node source_env)
                        Just t -> do
                            let sources_changed = mconcat $ map (.changed) done
                            needs_rebuild <- needsRebuild node
                            case (sources_changed, needs_rebuild) of
                                (Unchanged, False) -> return $ Just source_env
                                otherwise -> do
                                    (result, result_env) <- executeTask source_env t
                                    return if result then Just result_env else Nothing
        case status of
            Nothing  -> return $ Failed node
            Just env -> do
                changed <- decideNode decider node
                return $ Done node env changed
