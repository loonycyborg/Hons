{-# LANGUAGE ImplicitParams, BlockArguments, TypeFamilies, PatternSynonyms, ViewPatterns #-}
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
command :: (NodeListNonEmpty a, NodeList b) => Action vars -> ActionM vars [ByteString] -> a -> b -> RuleSet vars
command action sign target source = RuleSet (HM.fromList $ map (, task) tlist) (connect tgraph sgraph) where
    tgraph = vertices tlist
    sgraph = vertices slist
    tlist = toList target
    slist = toList source
    task = Task (toNonEmpty target) slist action sign

propagateIO :: (NodeList a) => String -> a -> Evaluator vars -> RuleSet vars
propagateIO name targets transform = RuleSet (HM.singleton node (Propagator node transform)) (connect (vertices $ toList targets) (vertex node)) where
    node = mkValue name

propagate :: (NodeList a) => String -> a -> ((?target :: Node) => Environment vars -> Environment vars) -> RuleSet vars
propagate name targets transform = propagateIO name targets do (getenv >>= put . transform) >> return noResult

data BuilderF vars t where
    BuilderF   :: ([Node] -> RuleSet vars) -> NonEmpty Node -> [t] -> BuilderF vars t
    SourceF    :: NonEmpty Node -> BuilderF vars t
    PropagateF :: ((?target :: Node) => Environment vars -> Environment vars) -> BuilderF vars t

data Builder vars where
    Builder   :: (t -> s -> RuleSet vars) -> (t -> NonEmpty Node) -> ([Node]->s) -> t -> [Builder vars]-> Builder vars
    Source    :: NodeListNonEmpty a => a -> Builder vars
    Propagate :: ((?target :: Node) => Environment vars -> Environment vars) -> Builder vars

instance MuRef (Builder vars) where
    type DeRef (Builder vars) = BuilderF vars
    mapDeRef _ (Source nodes)                              = pure $ SourceF (toNonEmpty nodes)
    mapDeRef f (Builder builder targetF sourceF tgts srcs) = BuilderF (builder tgts . sourceF) (targetF tgts) <$> traverse f srcs
    mapDeRef _ (Propagate transform)                       = pure $ PropagateF transform

reifyBuilderChain :: (DeRef s ~ BuilderF vars, MuRef s) => s -> IO (RuleSet vars)
reifyBuilderChain g = reifyToRuleset <$> reifyGraph g

reifyBuilderChains :: (DeRef s ~ BuilderF vars, MuRef s, Traversable t) => t s -> IO (RuleSet vars)
reifyBuilderChains g = foldMap reifyToRuleset <$> reifyGraphs g

reifyToRuleset :: Graph (BuilderF vars) -> RuleSet vars
reifyToRuleset x = gs where
    Graph graph _ = x
    builderGraph (_, SourceF t) = RuleSet HM.empty $ vertices $ toList t
    builderGraph (_, BuilderF builder t s) = builder $ concatMap (toList . (nodemap HM.!)) s
    builderGraph (s, PropagateF transform) = RuleSet (HM.singleton node (Propagator node do (getenv >>= put . transform) >> return noResult)) (vertex node) where
        node = NE.head $ nodemap HM.! s
    gs = foldMap builderGraph graph
    nodemap = HM.fromList $ fmap builderTarget graph
    builderTarget (n, BuilderF _ t _) = (n, t)
    builderTarget (n, SourceF t)      = (n, t)
    builderTarget (n, PropagateF _)   = (n, mkValue ("propagator" <> show n) :| [])

pattern Command :: NodeListNonEmpty a => Action vars -> ActionM vars [ByteString] -> a -> [Builder vars] -> Builder vars
pattern Command <- (const False -> True) where
        Command action sigaction t s = Builder (command action sigaction) toNonEmpty id t s
