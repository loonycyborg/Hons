{-# LANGUAGE OverloadedRecordDot #-}
module DepGraph where
import Node ( Node )
import Taskmaster (Task(Task))
import Algebra.Graph.AdjacencyMap
    ( connect, overlay, overlays, vertex, vertices, AdjacencyMap, induce, empty )
import Algebra.Graph.AdjacencyMap.Algorithm (reachable, topSort)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as L

type DepGraph = AdjacencyMap Node
type TaskList vars = HM.HashMap Node (Task vars)

data RuleSet vars where
    RuleSet :: { tasks :: TaskList vars, graph :: DepGraph } -> RuleSet vars
    deriving (Eq, Show)

combine :: RuleSet vars -> RuleSet vars -> RuleSet vars
combine r1 r2 = RuleSet (HM.unionWithKey handleDuplicates r1.tasks r2.tasks) (overlay r1.graph r2.graph) where
    handleDuplicates k t1 t2 = error $ "Multiple ways to build node " ++ show k ++ "specified"

instance Semigroup (RuleSet vars) where
    (<>) = combine

instance Monoid (RuleSet vars) where
    mempty = RuleSet HM.empty empty

buildOrder :: RuleSet vars -> Node -> L.NonEmpty (Node, Maybe (Task vars))
buildOrder ruleset goal = order where
    componentVertices = Set.fromList (reachable ruleset.graph goal)
    component = induce (`Set.member` componentVertices) ruleset.graph
    order = L.reverse $ case topSort component of
        Left cycle -> error ("Dependency cycle detected: " ++ show cycle)
        Right l    -> L.map mkItem (L.fromList l)
    mkItem n = (n, HM.lookup n ruleset.tasks)