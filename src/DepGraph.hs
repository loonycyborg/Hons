{-# LANGUAGE OverloadedRecordDot #-}
module DepGraph where
import Node ( Node )
import Action (Task(Task))
import Algebra.Graph.AdjacencyMap
    ( connect, overlay, overlays, vertex, vertices, AdjacencyMap, induce, empty, postSet )
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as L
import Data.Semigroup (sconcat)

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
buildOrder ruleset goal = L.reverse $ L.map mkItem $ topologicalSort ruleset.graph goal where
    mkItem n = (n, HM.lookup n ruleset.tasks)

data VertexSearchState = Discovered | Finished deriving (Show, Eq)
type SearchState a n = HM.HashMap n (VertexSearchState, a)

depthFirstFold :: (a1 -> Node -> a1) -> (a1 -> [a2] -> [a1] -> [a1] -> a2) -> DepGraph -> Node -> a1 -> a2
depthFirstFold discover_func finish_func graph vertex a1 =
    fst $ go HM.empty vertex (discover_func a1 vertex) where
        go search vertex a1 = finish_vertex $ foldr check_edge (HM.insert vertex (Discovered, a1) search, [], [], []) (postSet vertex graph)
            where
            check_edge target_vertex (search, tree_edges, front_cross_edges, back_edges) = case HM.lookup target_vertex search of
                Nothing               -> (search',a2:tree_edges,    front_cross_edges,    back_edges) where
                    (a2, search') = go search target_vertex (discover_func a1 target_vertex)
                Just (Discovered, a1) -> (search,    tree_edges,    front_cross_edges, a1:back_edges)
                Just (Finished, a1)   -> (search,    tree_edges, a1:front_cross_edges,    back_edges)
            finish_vertex (search, tree_edges, front_cross_edges, back_edges) =
                (finish_func a1 tree_edges front_cross_edges back_edges, HM.adjust (\(Discovered, x) -> (Finished, x)) vertex search)

topologicalSort :: DepGraph -> Node -> L.NonEmpty Node
topologicalSort graph vertex = depthFirstFold (\l n -> n:l) topS graph vertex [] where
    topS (x:_) xs _ []         = sconcat $ L.singleton x L.:| xs
    topS xs    _  _ ((b:_):bs) = error $ "Dependency cycle detected: " ++ (show $ b : (reverse $ b : takeWhile (/=b) xs))
