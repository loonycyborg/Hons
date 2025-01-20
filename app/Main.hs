{-# LANGUAGE OverloadedRecordDot, QuasiQuotes, DataKinds, TypeApplications #-}
module Main where

import qualified Hons (someFunc)
import qualified Rule
import qualified DepGraph
import qualified Environment
import qualified Taskmaster
import Environment
import Node (filelist, mkFsNode, mkPropagator)

import Algebra.Graph.Export.Dot
import GHC.IO (unsafePerformIO)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Maybe

envp =
  envVar @"jobs"    (0 :: Int)      :+:
  envVar @"c.flags" ([] ::[String]) :+:
  envVar @"c.cc"    "gcc"           :+:
  EnvNihil

env = makeEnv envp
-- In referentially transparent languages like Haskell there is no such thing
-- as modifiable variables so "env" will have same value throughout the source file
-- therefore standared SCons practice of calling env.Append(..) won't work
-- so we'll use propagator nodes instead

targets = [filelist|prog|]
sources = [filelist|src1.c src2.c|]
objects = [filelist|src1.o src2.o|]
p = Rule.Rule targets objects (return True)
r = Rule.RuleChain p (zipWith mkO objects sources)

-- Propagator nodes modify environment associated with them
-- In Hons each node has own environment associated with it
-- that is calculated by merging the node's source environments
-- therefore all environment overrides such as those introduced
-- by propagator will carry over upstream
propagator1 = mkPropagator "test1" env (do
  State.modify $ eReplace @"c.flags" ["-funroll-loops"]
  )
propagator2 = mkPropagator "test2" env (do
  State.modify $ eReplace @"c.flags" ["-Ofast"]
  )

prule = [
          Rule.Depends (head sources) propagator1,
          Rule.Depends (last sources) propagator2
        ]
cyc = Rule.Rule sources targets (return True)

mkO o c = Rule.Rule [o] [c] (return True)
(g, t) = DepGraph.applyRules $ r : prule

order = DepGraph.buildOrder (g, t) (head targets)

main :: IO ()
main = do
  writeFile "graph.dot" (exportViaShow g)
  print order
  print t
  print (map (Taskmaster.taskContext g t env) (mapMaybe snd order))
