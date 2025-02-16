{-# LANGUAGE GADTs, OverloadedRecordDot, InstanceSigs, FlexibleContexts #-}
module Taskmaster (module Taskmaster, liftIO) where
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.List.NonEmpty as L
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Type.Reflection
import Algebra.Graph.AdjacencyMap

import Node
import Environment
import Data.Maybe (mapMaybe, fromJust, isJust, isNothing)

type ActionM vars t = StateT (Environment vars) (ReaderT (Task vars) IO) t
type Action vars = (ActionM vars) Bool

data Task vars where
    Task :: { targets :: [Node], sources :: [Node], action :: Action vars } -> Task vars

instance Eq (Task vars) where
    (==) :: Task vars -> Task vars -> Bool
    (==) t1 t2 = head t1.targets == head t2.targets

instance Show (Task vars) where
    show (Task targets _ _) = "[[[" ++ (show . head $ targets) ++ "]]]"

data TaskStatus = Pending | Done | Failed deriving (Eq, Show, Enum)

data ExecutionContext vars where
    ExecutionContext :: { target :: Node, env :: Maybe (Environment vars), task :: Maybe (Task vars), status :: TaskStatus } -> ExecutionContext vars deriving Show

executeTask :: Typeable vars => Environment vars -> Task vars -> IO (Bool, Environment vars)
executeTask env task@(Task targets sources action) =
    runReaderT (runStateT action env) task

gett :: ActionM vars (Task vars)
gett = lift ask
getenv :: ActionM vars (Environment vars)
getenv = get
putenv :: Environment vars -> ActionM vars ()
putenv = put
modenv :: (Environment vars -> Environment vars) -> ActionM vars ()
modenv = modify

transformWithNode :: Typeable vars => Node -> Environment vars -> Environment vars
transformWithNode (ValueNode _ _ tr) = eTransform tr
transformWithNode (FsNode _) = eTransform EIdentity

contextList deps env = L.map mkCtx where
    mkCtx (node, Nothing) = ExecutionContext node (if isLeaf then Just (transformWithNode node env) else Nothing) Nothing (if isLeaf then Done else Pending) where
        isLeaf = null $ postSet node deps
    mkCtx (node, Just task) = ExecutionContext node Nothing (Just task) Pending

type Contexts vars = HM.HashMap Node (ExecutionContext vars)
type ContextList vars = L.NonEmpty (ExecutionContext vars)

execute :: (Typeable vars) => AdjacencyMap Node -> ContextList vars -> StateT (Contexts vars) IO (ContextList vars)
execute deps = mapM execute_context where
    execute_context context@(ExecutionContext node env task status) = do
        ctx <- get
        case task of
            Nothing -> 
              let
                source_ctx = fromJust $ lookup_ctx ctx [node]
                failed = src_failed source_ctx
                done = src_done source_ctx
                new_status
                 | status /= Pending = status
                 | done      = Done
                 | failed    = Failed
                 | otherwise = Pending
                new_env
                 | isJust env = env
                 | new_status == Done = Just $ source_env source_ctx
                 | otherwise  = Nothing
                new_context = ExecutionContext node new_env Nothing new_status
              in do
                modify $ HM.insert node new_context
                return new_context
            Just t ->
              let
                source_ctx_m = lookup_ctx ctx t.targets
                incomplete_src = isNothing source_ctx_m
                source_ctx = fromJust source_ctx_m
                pre_failed = src_failed source_ctx
                ready = src_done source_ctx
                src_env = source_env source_ctx
                new_env
                 | isJust env = env
                 | ready = Just src_env
                 | otherwise = Nothing
                new_status result
                 | incomplete_src = Pending
                 | status /= Pending = status
                 | pre_failed = Failed
                 | result = Done
                 | otherwise = Failed
                new_context result = ExecutionContext node new_env task (new_status result)
              in do
                (result, result_env) <- if not incomplete_src && status == Pending && ready then liftIO (executeTask (fromJust new_env) t) else return (False, fromJust env)
                modify $ HM.insert node $ new_context result
                return $ new_context result
        where
            lookup_ctx ctx targets = sequence $ lookup_src ctx $ sources_t targets
            lookup_src ctx = map (`HM.lookup` ctx) . S.toList
            sources target = postSet target deps
            sources_t targets = S.unions $ map sources targets
            source_env src = transformWithNode node $ foldr1 eMerge $ mapMaybe ((.env)) src
            src_failed src = Failed `elem` map (.status) src
            src_done = all ((==Done) . (.status))

build :: Typeable vars => AdjacencyMap Node -> Environment vars -> L.NonEmpty (Node, Maybe (Task vars)) -> IO (ContextList vars, Contexts vars)
build deps env nodes = runStateT (execute deps $ contextList deps env nodes) HM.empty
