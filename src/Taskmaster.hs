{-# LANGUAGE OverloadedRecordDot #-}
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
    Task :: { targets :: L.NonEmpty Node, sources :: [Node], action :: Action vars } -> Task vars

instance Eq (Task vars) where
    (==) :: Task vars -> Task vars -> Bool
    (==) t1 t2 = L.head t1.targets == L.head t2.targets

instance Show (Task vars) where
    show (Task targets _ _) = "[[[" ++ (show . L.head $ targets) ++ "]]]"

data ExecutionContext vars where
    Pending :: { target :: Node, task :: Maybe (Task vars) } -> ExecutionContext vars
    Ready   :: { target :: Node, env :: Environment vars, ready_task :: Task vars } -> ExecutionContext vars
    Done    :: { target :: Node, env :: Environment vars } -> ExecutionContext vars
    Failed  :: { target :: Node } -> ExecutionContext vars
    deriving Show

classifyCtx :: Foldable t => t (ExecutionContext vars) -> ([ExecutionContext vars], [ExecutionContext vars], [ExecutionContext vars])
classifyCtx = foldr classifyContext ([], [], []) where
    classifyContext c (lDone, lFailed, lUnbuilt) =
        case c of
            Done {}   -> (c:lDone,   lFailed,   lUnbuilt)
            Failed {} -> (  lDone, c:lFailed,   lUnbuilt)
            _         -> (  lDone,   lFailed, c:lUnbuilt)

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
    mkCtx (node, Nothing) = if isLeaf then Done node (transformWithNode node env) else Pending node Nothing where
        isLeaf = null $ postSet node deps
    mkCtx (node, Just task) = Pending node (Just task)

type Contexts vars = HM.HashMap Node (ExecutionContext vars)
type ContextList vars = L.NonEmpty (ExecutionContext vars)

transformContext :: Typeable vars => AdjacencyMap Node -> Contexts vars -> ExecutionContext vars -> ExecutionContext vars
transformContext deps ctx context =
    let
        sources target = postSet target deps
        sources_t targets = S.unions $ map sources targets
        lookup_src ctx = map (`HM.lookup` ctx) . S.toList
        lookup_ctx ctx targets = sequence $ lookup_src ctx $ sources_t targets
        source_ctx = lookup_ctx ctx (case context of
            Pending target (Just t) -> L.toList t.targets
            _ -> [context.target])
        src_complete = isJust source_ctx
        src = fromJust source_ctx
        (src_done, src_failed, src_unbuilt) = classifyCtx src
        source_env = foldr1 eMerge $ map ((.env)) src_done
        src_context
          | not src_complete = Pending context.target context.task
          | not $ null src_failed = Failed context.target
          | null src_unbuilt = Done context.target source_env
          | otherwise = Pending context.target context.task
    in
        case (src_context, context) of
            (_, Failed target) -> Failed target
            (Failed {}, _) -> src_context
            (Pending {}, _) -> src_context
            (Done _ env, Pending target (Just ready_task)) -> Ready target env ready_task
            (Done _ env, Pending target Nothing) -> Done target (transformWithNode target env)
            _ -> context

execute :: (Typeable vars) => AdjacencyMap Node -> ContextList vars -> StateT (Contexts vars) IO (ContextList vars)
execute deps = mapM execute_context where
    execute_context context = do
        ctx <- get
        let src_context = transformContext deps ctx context
        case src_context of
            Ready target env ready_task -> do
                (result, result_env) <- liftIO $ executeTask env ready_task
                let result_context = if result then Done src_context.target result_env else Failed src_context.target
                modify $ HM.insert src_context.target result_context
                return result_context
            _ -> do
                modify $ HM.insert src_context.target src_context
                return src_context

build :: Typeable vars => AdjacencyMap Node -> Environment vars -> L.NonEmpty (Node, Maybe (Task vars)) -> IO (ContextList vars, Contexts vars)
build deps env nodes = runStateT (execute deps $ contextList deps env nodes) HM.empty
