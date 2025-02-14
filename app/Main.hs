{-# LANGUAGE OverloadedRecordDot, QuasiQuotes, DataKinds, TypeApplications, BlockArguments #-}
module Main where

import qualified Hons (someFunc)
import qualified DepGraph
import qualified Environment
import qualified Taskmaster
import Environment
import Node (filelist, mkFsNode, mkPropagator)
import Builder

import Algebra.Graph.Export.Dot
import GHC.IO (unsafePerformIO)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Maybe

envp =
  envVar @"jobs"    (0 :: Int)      :+:
  envVar @"c.flags" ([] ::[String]) :+:
  envVar @"c.cc"    "gcc"           :+:
  envVar @"switch"  (Nothing :: Maybe Bool) :+:
  EnvNihil

env = makeEnv envp
-- In referentially transparent languages like Haskell there is no such thing
-- as modifiable variables so "env" will have same value throughout the source file
-- therefore standared SCons practice of calling env.Append(..) won't work
-- so we'll use propagator nodes instead

targets = [filelist|prog|]
sources = [filelist|src1.c src2.c|]
objects = [filelist|src1.o src2.o|]
p = command targets objects do
  print "Pretending to build program"
  return True
r = p <> mconcat (zipWith mkO objects sources)

-- Propagator nodes modify environment associated with them
-- In Hons each node has own environment associated with it
-- that is calculated by merging the node's source environments
-- therefore all environment overrides such as those introduced
-- by propagator will carry over upstream

prule =
  propagate "test1" env [head sources] do
    State.modify $ eReplace @"c.flags" ["-funroll-loops"]
    State.modify $ eReplace @"switch" (Just True)
  <>
  propagate "test2" env [last sources] do
    State.modify $ eReplace @"c.flags" ["-Ofast"]
cyc = command sources targets (return True)

mkO o c = command o c do
  print "Pretending to build object"
  return True

rules = r <> prule
g = rules.graph
t = rules.tasks

order = DepGraph.buildOrder rules (head targets)

main :: IO ()
main = do
  writeFile "graph.dot" (exportViaShow g)
  print t
  print order
  result <- Taskmaster.build g env order
  print result
