module Builder where
import DepGraph
import Node
import Rule

depends = Depends
command = Rule
propagate name env targets action = map (`depends` propagator) targets where
    propagator = mkPropagator name env action