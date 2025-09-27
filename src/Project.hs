module Project where
import Node
import Environment
import DepGraph
import Type.Reflection (Typeable)

data Project where
    Project :: forall vars . Typeable vars => { env :: Environment vars, rules :: RuleSet vars, defaultTargets :: [Node] } -> Project
