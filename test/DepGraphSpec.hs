module DepGraphSpec (spec) where

import Test.Syd
import Test.QuickCheck
import Algebra.Graph
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Algebra.Graph.ToGraph (ToGraph(toAdjacencyMap))
import Data.Bool
import Data.Either
import Control.Exception

import DepGraph
import Node

instance (Arbitrary a, Ord a) => Arbitrary (Graph a) where
    arbitrary = overlay <$> (vertices <$> arbitrary) <*> (edges <$> arbitrary)

spec :: Spec
spec = do
    describe "DepthFirstFold algorithm" $ do
        it "topsorts correctly" $
            property $ \(g :: Graph Int) ->
                let depgraph :: DepGraph = AM.gmap (mkValue . show) $ toAdjacencyMap g
                    edges = AM.edgeList depgraph
                    verts = AM.vertexSet depgraph
                    subgraph_verts = bool (AMA.reachable depgraph (minimum verts)) [] $ Set.null verts
                    subgraph = AM.induce (`elem` subgraph_verts) depgraph
                    dffold_sort = bool (NE.toList $ topologicalSort depgraph (minimum verts)) [] $ Set.null verts
                    alga_sort = AMA.topSort subgraph
                in
                    counterexample ("Topological sort: " <> if isLeft alga_sort then "<cycle>" else show dffold_sort) $
                    case alga_sort of
                        Left _ -> evaluate (last dffold_sort) `shouldThrow` anyErrorCall
                        _      -> AMA.isTopSortOf dffold_sort subgraph `shouldBe` True
