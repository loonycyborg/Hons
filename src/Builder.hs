module Builder where
import Algebra.Graph.AdjacencyMap
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty, fromList)
import DepGraph
import Node
import Action
import qualified Algebra.Graph
import Algebra.Graph.ToGraph (ToGraph (toAdjacencyMap, ToVertex, vertexList))
import Environment
import Type.Reflection (Typeable)

depends :: (NodeList a, NodeList b) => a -> b -> RuleSet vars
depends target source = RuleSet HM.empty (connect (vertices $ toList target) (vertices $ toList source))
command :: (NodeListNonEmpty a, NodeList b) => a -> b -> Action vars -> RuleSet vars
command target source action = RuleSet (HM.fromList $ map (, task) tlist) (connect tgraph sgraph) where
    tgraph = vertices tlist
    sgraph = vertices slist
    tlist = toList target
    slist = toList source
    task = Task (toNonEmpty target) slist action
propagateG :: (NodeList a, EnvTransform p) => String -> a -> p -> RuleSet vars
propagateG name targets action = mconcat $ map (`depends` propagator) (toList targets) where
    propagator = mkPropagator name action

propagate :: (NodeList a, Typeable vars) => String -> a -> (Environment vars -> Environment vars) -> RuleSet vars
propagate = propagateG
