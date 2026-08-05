{-# LANGUAGE ImplicitParams, BlockArguments, TypeFamilies, DataKinds, TemplateHaskellQuotes, TypeAbstractions #-}
module Builder where
import Algebra.Graph.AdjacencyMap
import Control.Monad (join)
import qualified Data.HashMap.Strict as HM
import qualified Data.TMap as TM
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
import Language.Haskell.TH.Quote

depends :: (NodeList a, NodeList b) => a -> b -> RuleSet vars
depends target source = RuleSet HM.empty (connect (vertices $ toList target) (vertices $ toList source))
emptyRuleSet :: RuleSet vars
emptyRuleSet = depends ([] @Node) ([] @Node)
command :: (NodeListNonEmpty a, NodeList b) => ActionTask vars -> ActionSig vars -> a -> b -> RuleSet vars
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
    BuilderF   :: Tag -> ((?tags::TagRegistry) => NonEmpty Node -> RuleSet vars) -> NonEmpty Node -> NonEmpty t -> BuilderF vars t
    SourceF    :: Tag -> NonEmpty Node -> [t] -> BuilderF vars t
    PropagateF :: ActionEval vars -> String -> BuilderF vars t

data ChainType = BuilderC | PropagatorC deriving Show

data Builder vars (ct :: ChainType) where
    Builder     :: Tag -> (t -> s -> RuleSet vars) -> (t -> NonEmpty Node) -> ((?tags::TagRegistry) => NonEmpty Node -> s) -> t -> NonEmpty (Builder vars BuilderC) -> Builder vars BuilderC
    Source      :: NodeListNonEmpty a => Tag -> a -> [Builder vars PropagatorC] -> Builder vars BuilderC
    Propagate   :: ((?target :: Node) => Environment vars -> Environment vars) -> Builder vars PropagatorC
    PropagateIO :: ActionEval vars -> String -> Builder vars PropagatorC

instance MuRef (Builder vars ct) where
    type DeRef (Builder vars ct) = BuilderF vars
    mapDeRef f (Source tag nodes propagators)                  = SourceF tag (toNonEmpty nodes) <$> traverse f propagators
    mapDeRef f (Builder tag builder targetF sourceF tgts srcs) = BuilderF tag (builder tgts . sourceF) (targetF tgts) <$> traverse f srcs
    mapDeRef _ (Propagate transform)                           = pure $ PropagateF (do (getenv >>= put . transform) >> return (noResult, [])) "propagator"
    mapDeRef _ (PropagateIO evaluator name)                    = pure $ PropagateF evaluator name

reifyBuilderChain :: (DeRef s ~ BuilderF vars, MuRef s) => s -> IO (RuleSet vars)
reifyBuilderChain g = reifyToRuleset <$> reifyGraph g

reifyBuilderChains :: (DeRef s ~ BuilderF vars, MuRef s, Traversable t) => t s -> IO (RuleSet vars)
reifyBuilderChains g = foldMap reifyToRuleset <$> reifyGraphs g

reifyToRuleset :: Graph (BuilderF vars) -> RuleSet vars
reifyToRuleset x = gs where
    Graph graph _ = x
    builderGraph (_, SourceF _ t ps)         = depends t $ ps >>= (toList . (nodemap HM.!))
    builderGraph (_, BuilderF _ builder t s) = let ?tags=tag_registry in builder $ s >>= (nodemap HM.!)
    builderGraph (s, PropagateF evaluator _) = RuleSet (HM.singleton node (Evaluator node [] evaluator (return []))) (vertex node) where
        node = NE.head $ nodemap HM.! s
    gs = foldMap builderGraph graph
    nodemap = HM.fromList $ fmap builderTarget graph
    tag_registry :: TagRegistry
    tag_registry = HM.foldMapWithKey (flip mk_node_tag_registry) $ HM.fromList $ fmap builderTag graph
    builderTarget (n, BuilderF _ _ t _) = (n, t)
    builderTarget (n, SourceF _ t _)    = (n, t)
    builderTarget (n, PropagateF _ p) = (n, mkValue (p <> show n) :| [])
    builderTag (n, BuilderF t _ _ _) = (n, mk_tag_registry t)
    builderTag (n, SourceF t _ _)    = (n, mk_tag_registry t)
    builderTag (n, PropagateF {}) = (n, mk_tag_registry TagNihil)
    mk_tag_registry = flip insertTag TM.empty
    mk_node_tag_registry tag = HM.fromList . NE.toList . fmap (,tag) . (nodemap HM.!)

data Tag where
    TagNihil :: Tag
    (:#)     :: Typeable a => a -> Tag -> Tag
infixr 5 :#

type TagRegistry = HM.HashMap Node TM.TMap

insertTag :: Tag -> TM.TMap -> TM.TMap
insertTag TagNihil registry = registry
insertTag (a :# rest) registry = TM.insert a registry <> insertTag rest registry

tag :: (Typeable a, ?tags::TagRegistry) => Node -> Maybe a
tag @a node = join $ TM.lookup @a <$> HM.lookup node ?tags

ioBuilder :: NodeListNonEmpty t => ((?e::Environment vars, ?t::Task vars) => IO Bool) -> ((?e::Environment vars, ?t::Task vars) => IO [ByteString]) -> t -> NonEmpty (Builder vars BuilderC) -> Builder vars BuilderC
ioBuilder action sigaction = Builder TagNihil (command actionM sigactionM) toNonEmpty id where
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

sourcelist :: QuasiQuoter
sourcelist = QuasiQuoter
    { quoteExp  = \s -> case words s of
        n1 : ns -> [| Source TagNihil nodes |] where nodes = fmap mkFsNodeFromString $ n1 :| ns
        _       -> error "empty sourcelist"
    , quotePat  = error "sourcelist quasiquoter doesn't support use as pattern"
    , quoteType = error "sourcelist quasiquoter doesn't support use as type"
    , quoteDec  = error "sourcelist quasiquoter doesn't support use as declaration"
    }

aliasValue :: String -> [Builder vars PropagatorC] -> Builder vars BuilderC
aliasValue name = Source TagNihil (mkValue name)
