module DepGraph where
import Node ( Node )
import Rule ( Rule(RuleChain, Rule, Depends) )
import Taskmaster (Task(Task))
import Algebra.Graph.AdjacencyMap
    ( connect, overlay, overlays, vertex, vertices, AdjacencyMap, induce )
import Algebra.Graph.AdjacencyMap.Algorithm (reachable, topSort)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HS
import qualified Control.Monad.Trans.State.Strict as State
    ( runState, modify, State, get )

type DepGraph = AdjacencyMap Node
type TaskList = HS.HashMap Node Task

applyRule :: Rule -> State.State TaskList DepGraph
applyRule (Rule targets sources _) = do
    mapM_ updateTaskList targets
    return $ connect (vertices targets) (vertices sources) 
    where
        updateTaskList node = State.modify (\tl ->
            if HS.member node tl then
               error "Multiple tasks associated with same target node" else
               HS.insert node mkTask tl)
        mkTask = Task targets sources (return True)
applyRule (RuleChain rule rules) = do
    r <- applyRule rule
    rs <- mapM applyRule rules
    return $ overlay r (overlays rs)
applyRule (Depends target source) = do
    return $ connect (vertex target) (vertex source)

applyRules :: [Rule] -> (DepGraph, TaskList)
applyRules rules = (overlays depGraph, taskList) where
    (depGraph, taskList) = State.runState (mapM applyRule rules) HS.empty

buildOrder :: (DepGraph, TaskList) -> Node -> [(Node, Maybe Task)]
buildOrder (depGraph, taskList) goal = order where
    componentVertices = Set.fromList (reachable depGraph goal)
    component = induce (`Set.member` componentVertices) depGraph
    order = reverse $ case topSort component of
        Left cycle -> error ("Dependency cycle detected: " ++ show cycle)
        Right l    -> map mkItem l
    mkItem n = (n, HS.lookup n taskList)