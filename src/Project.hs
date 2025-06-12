module Project where
import Environment
import DepGraph
import Type.Reflection (Typeable)

data Project where
    Project :: forall vars . Typeable vars => { env :: Environment vars, rules :: RuleSet vars } -> Project