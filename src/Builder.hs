{-# LANGUAGE ImplicitParams #-}
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
import Data.ByteString (ByteString)

depends :: (NodeList a, NodeList b) => a -> b -> RuleSet vars
depends target source = RuleSet HM.empty (connect (vertices $ toList target) (vertices $ toList source))
command :: (NodeListNonEmpty a, NodeList b) => a -> b -> Action vars -> ActionM vars [ByteString] -> RuleSet vars
command target source action sign = RuleSet (HM.fromList $ map (, task) tlist) (connect tgraph sgraph) where
    tgraph = vertices tlist
    sgraph = vertices slist
    tlist = toList target
    slist = toList source
    task = Task (toNonEmpty target) slist action sign

propagateIO :: (NodeList a, Typeable vars) =>  String -> a -> Evaluator vars -> RuleSet vars
propagateIO name targets transform = RuleSet (HM.singleton node (Propagator node transform)) (connect (vertices $ toList targets) (vertex node)) where
    node = mkValue name

propagate :: (NodeList a, Typeable vars) =>  String -> a -> ((?target :: Node) => Environment vars -> Environment vars) -> RuleSet vars
propagate name targets transform = propagateIO name targets (pure . (,noResult) . transform)
