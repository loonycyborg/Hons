{-# LANGUAGE ImplicitParams, BlockArguments, TypeFamilies, DataKinds #-}
module Builder where
import Algebra.Graph.AdjacencyMap
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import qualified Data.List.NonEmpty as NE
import Data.Reify
import DepGraph
import Node
import Action
import qualified Algebra.Graph
import Algebra.Graph.ToGraph (ToGraph (toAdjacencyMap, ToVertex, vertexList))
import Environment
import Type.Reflection (Typeable)
import Data.ByteString (ByteString)

depends :: (NodeList a, NodeList b) => a -> b -> RuleSet vars
depends target source = RuleSet HM.empty (connect (vertices $ toList target) (vertices $ toList source))
emptyRuleSet :: RuleSet vars
emptyRuleSet = depends ([] @Node) ([] @Node)
command :: (NodeListNonEmpty a, NodeList b) => Action vars -> ActionSig vars -> a -> b -> RuleSet vars
command action sign target source = RuleSet (HM.fromList $ map (, task) tlist) (connect tgraph sgraph) where
    tgraph = vertices tlist
    sgraph = vertices slist
    tlist = toList target
    slist = toList source
    task = Task (toNonEmpty target) slist action sign

evaluate :: NodeList a => Node -> a -> ActionEval vars -> ActionSig vars -> RuleSet vars
evaluate target sources eval sign = RuleSet (HM.singleton target $ Evaluator target (toList sources) eval sign) (connect (vertex target) (vertices $ toList sources))

propagateIO :: (NodeList a) => String -> a -> ActionEval vars -> RuleSet vars
propagateIO name targets transform = RuleSet (HM.singleton node (Evaluator node [] transform (return []))) (connect (vertices $ toList targets) (vertex node)) where
    node = mkValue name

propagate :: (NodeList a) => String -> a -> ((?target :: Node) => Environment vars -> Environment vars) -> RuleSet vars
propagate name targets transform = propagateIO name targets do (getenv >>= put . transform) >> return (noResult, [])

data BuilderF vars t where
    BuilderF   :: (NonEmpty Node -> RuleSet vars) -> NonEmpty Node -> NonEmpty t -> BuilderF vars t
    SourceF    :: NonEmpty Node -> [t] -> BuilderF vars t
    PropagateF :: ActionEval vars -> String -> BuilderF vars t

data ChainType = BuilderC | PropagatorC deriving Show

data Builder vars (ct :: ChainType) where
    Builder     :: (t -> s -> RuleSet vars) -> (t -> NonEmpty Node) -> (NonEmpty Node -> s) -> t -> NonEmpty (Builder vars BuilderC) -> Builder vars BuilderC
    Source      :: NodeListNonEmpty a => a -> [Builder vars PropagatorC] -> Builder vars BuilderC
    Propagate   :: ((?target :: Node) => Environment vars -> Environment vars) -> Builder vars PropagatorC
    PropagateIO :: ActionEval vars -> String -> Builder vars PropagatorC

instance MuRef (Builder vars ct) where
    type DeRef (Builder vars ct) = BuilderF vars
    mapDeRef f (Source nodes propagators)                  = SourceF (toNonEmpty nodes) <$> traverse f propagators
    mapDeRef f (Builder builder targetF sourceF tgts srcs) = BuilderF (builder tgts . sourceF) (targetF tgts) <$> traverse f srcs
    mapDeRef _ (Propagate transform)                       = pure $ PropagateF (do (getenv >>= put . transform) >> return (noResult, [])) "propagator"
    mapDeRef _ (PropagateIO evaluator name)                = pure $ PropagateF evaluator name

reifyBuilderChain :: (DeRef s ~ BuilderF vars, MuRef s) => s -> IO (RuleSet vars)
reifyBuilderChain g = reifyToRuleset <$> reifyGraph g

reifyBuilderChains :: (DeRef s ~ BuilderF vars, MuRef s, Traversable t) => t s -> IO (RuleSet vars)
reifyBuilderChains g = foldMap reifyToRuleset <$> reifyGraphs g

reifyToRuleset :: Graph (BuilderF vars) -> RuleSet vars
reifyToRuleset x = gs where
    Graph graph _ = x
    builderGraph (_, SourceF t ps)           = depends t $ ps >>= (toList . (nodemap HM.!))
    builderGraph (_, BuilderF builder t s)   = builder $ s >>= (nodemap HM.!)
    builderGraph (s, PropagateF evaluator _) = RuleSet (HM.singleton node (Evaluator node [] evaluator (return []))) (vertex node) where
        node = NE.head $ nodemap HM.! s
    gs = foldMap builderGraph graph
    nodemap = HM.fromList $ fmap builderTarget graph
    builderTarget (n, BuilderF _ t _) = (n, t)
    builderTarget (n, SourceF t _)    = (n, t)
    builderTarget (n, PropagateF _ p) = (n, mkValue (p <> show n) :| [])

ioBuilder :: NodeListNonEmpty t => ((?e::Environment vars, ?t::Task vars) => IO Bool) -> ((?e::Environment vars, ?t::Task vars) => IO [ByteString]) -> t -> NonEmpty (Builder vars BuilderC) -> Builder vars BuilderC
ioBuilder action sigaction = Builder (command actionM sigactionM) toNonEmpty id where
    actionM = do
        env <- getenv
        task <- gett
        let ?e = env
        let ?t = task
        liftIO action
    sigactionM = do
        env <- getenv
        task <- gett
        let ?e = env
        let ?t = task
        liftIO sigaction
