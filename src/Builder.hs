{-# LANGUAGE GADTs, TypeOperators, TupleSections #-}
module Builder where
import Algebra.Graph.AdjacencyMap
import qualified Data.HashMap.Strict as HM
import DepGraph
import Node
import Taskmaster
import qualified Algebra.Graph
import Algebra.Graph.ToGraph (ToGraph (toAdjacencyMap, ToVertex, vertexList))

depends :: (ToGraph a, ToVertex a ~ Node, ToGraph b, ToVertex b ~ Node) => a -> b -> RuleSet vars
depends target source = RuleSet HM.empty (connect (toAdjacencyMap target) (toAdjacencyMap source))
command :: (ToGraph a, ToVertex a ~ Node, ToGraph b, ToVertex b ~ Node) => a -> b -> Action vars -> RuleSet vars
command target source action = RuleSet (HM.fromList $ map (, task) tlist) (connect tgraph sgraph) where
    tgraph = toAdjacencyMap target
    sgraph = toAdjacencyMap source
    tlist = Algebra.Graph.ToGraph.vertexList target
    slist = Algebra.Graph.ToGraph.vertexList source
    task = Task tlist slist action
propagate name env targets action = mconcat $ map (`depends` propagator) targets where
    propagator = mkPropagator name env action