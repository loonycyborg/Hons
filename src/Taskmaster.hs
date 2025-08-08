{-# LANGUAGE OverloadedRecordDot, BlockArguments #-}
module Taskmaster where
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.List.NonEmpty as L
import Type.Reflection
import Algebra.Graph.AdjacencyMap

import Action
import Node
import Environment
import Decider
import Data.Maybe (mapMaybe, fromJust, isJust, isNothing)

data ExecutionContext vars where
    Pending :: { target :: Node, task :: Maybe (Task vars) } -> ExecutionContext vars
    Ready   :: { target :: Node, env :: Environment vars, ready_task :: Task vars } -> ExecutionContext vars
    Done    :: { target :: Node, env :: Environment vars, changed :: Ruling } -> ExecutionContext vars
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

transformWithNode :: Typeable vars => Node -> Environment vars -> Environment vars
transformWithNode (ValueNode _ _ tr) = eTransform tr
transformWithNode (FsNode _) = eTransform EIdentity

contextList :: Typeable vars => AdjacencyMap Node -> Environment vars -> L.NonEmpty (Node, Maybe (Task vars)) -> L.NonEmpty (ExecutionContext vars)
contextList deps env = L.map mkCtx where
    mkCtx (node, Nothing) = if isLeaf then Done node (transformWithNode node env) Undecided else Pending node Nothing where
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
          | null src_unbuilt = Done context.target source_env Undecided
          | otherwise = Pending context.target context.task
    in
        case (src_context, context) of
            (_, Failed target) -> Failed target
            (Failed {}, _) -> src_context
            (Pending {}, _) -> src_context
            (Done _ env _, Pending target (Just ready_task)) -> Ready target env ready_task
            (Done _ env _, Pending target Nothing) -> Done target (transformWithNode target env) Undecided
            _ -> context

execute :: (Typeable vars) => DeciderContext -> AdjacencyMap Node -> ContextList vars -> StateT (Contexts vars) IO (ContextList vars)
execute decider deps = mapM execute_context where
    execute_context context = do
        ctx <- get
        let src_context = transformContext deps ctx context
        run_context <- case src_context of
            Ready target env ready_task -> do
                let sources_changed = mconcat $ fmap ((.changed) . (ctx HM.!)) $ toList $ postSet target deps
                needs_rebuild <- liftIO $ needsRebuild target
                case (sources_changed, needs_rebuild) of
                    (Unchanged, False) -> return $ Done src_context.target env Unchanged
                    otherwise -> do
                        (result, result_env) <- liftIO $ executeTask env ready_task
                        return if result then Done src_context.target result_env Undecided else Failed src_context.target
            _ -> return src_context
        changed <- liftIO $ decideNode decider context.target
        let result_context = run_context { changed = changed }
        modify $ HM.insert result_context.target result_context
        return result_context

build :: Typeable vars => AdjacencyMap Node -> Environment vars -> L.NonEmpty (Node, Maybe (Task vars)) -> IO (ContextList vars, Contexts vars)
build deps env nodes = withDeciderContext "honsign.sqlite" \decider -> runStateT (execute decider deps $ contextList deps env nodes) HM.empty
