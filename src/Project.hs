module Project where
import Node
import Environment
import DepGraph
import Builder
import Type.Reflection (Typeable)

data Project vars where
    Project :: { env :: Environment vars, builders :: [Builder vars], rules :: RuleSet vars, defaultTargets :: [Node] } -> Project vars

defaultProject :: EnvProto vars -> Project vars
defaultProject env = Project (makeEnv env) [] mempty []

data ProjectHolder = forall vars . Typeable vars => ProjectHolder (Project vars)
