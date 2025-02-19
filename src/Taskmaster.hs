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

transformContext :: Typeable vars => AdjacencyMap Node -> Contexts vars -> ExecutionContext vars -> ExecutionContext vars
transformContext deps ctx context@(ExecutionContext node env task status) =
    let
        sources target = postSet target deps
        sources_t targets = S.unions $ map sources targets
        lookup_src ctx = map (`HM.lookup` ctx) . S.toList
        lookup_ctx ctx targets = sequence $ lookup_src ctx $ sources_t targets
        source_ctx = lookup_ctx ctx (case task of
            Just t -> t.targets
            Nothing -> [node])
        src_complete = isJust source_ctx
        src = fromJust source_ctx
        source_env = foldr1 eMerge $ mapMaybe ((.env)) src
        src_failed = Failed `elem` map (.status) src
        src_done = all ((==Done) . (.status)) src
        new_status
          | status /= Pending = status
          | not src_complete  = Pending
          | src_failed        = Failed
          | src_done          = if isJust task then Pending else Done
          | otherwise         = Pending
        new_env
          | isJust env       = env
          | not src_complete = Nothing
          | src_failed       = Nothing
          | src_done         = if isJust task then Just source_env else Just $ transformWithNode node source_env
          | otherwise        = Nothing
    in
        ExecutionContext node new_env task new_status

execute :: (Typeable vars) => AdjacencyMap Node -> ContextList vars -> StateT (Contexts vars) IO (ContextList vars)
execute deps = mapM execute_context where
    execute_context context@(ExecutionContext node env task status) = do
        ctx <- get
        let src_context = transformContext deps ctx context
        case task of
            Nothing -> do
                modify $ HM.insert node src_context
                return src_context
            Just t -> do
                (result, result_env) <- if src_context.status == Pending && isJust src_context.env then
                    liftIO (executeTask (fromJust src_context.env) t)
                        else
                    return (False, fromJust env)
                let new_context = ExecutionContext node (Just result_env) task (if result then Done else Failed)
                modify $ HM.insert node new_context
                return new_context

build :: Typeable vars => AdjacencyMap Node -> Environment vars -> L.NonEmpty (Node, Maybe (Task vars)) -> IO (ContextList vars, Contexts vars)
build deps env nodes = runStateT (execute deps $ contextList deps env nodes) HM.empty
