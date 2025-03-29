{-# LANGUAGE OverloadedRecordDot, QuasiQuotes, DataKinds, BlockArguments #-}
module Main where

import qualified Hons (someFunc)
import qualified DepGraph
import qualified Environment
import qualified Taskmaster
import CmdLine
import Environment
import Node (fs, filelist, mkFsNode, mkPropagator, Node(..))
import Builder
import qualified Tool.CC as CC

import Algebra.Graph.Export.Dot
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text.Short as TS
import qualified Data.List.NonEmpty as NE
import Data.Foldable (Foldable(fold))

envp =
  CC.toolEnv                                    +:
  envVar @"jobs"       (0 :: Int)              :+:
  envVar @"switch"     (Nothing :: Maybe Bool) :+:
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
  task <- Taskmaster.gett
  Taskmaster.liftIO $ print ("Pretending to build program: " ++ show task.targets ++ " -> " ++ show task.sources)
  return True
r = p <> fold (NE.zipWith mkO objects sources)

-- Propagator nodes modify environment associated with them
-- In Hons each node has own environment associated with it
-- that is calculated by merging the node's source environments
-- therefore all environment overrides such as those introduced
-- by propagator will carry over upstream

prule =
  propagate "test1" env [NE.head sources] do
    State.modify $ eReplace @"switch" (Just True)

cyc = command sources targets (return True)

mkO o c = command o c do
  task <- Taskmaster.gett
  Taskmaster.liftIO $ print ("Pretending to build object: " ++ show task.targets ++ " -> " ++ show task.sources)
  Taskmaster.liftIO $ spawnCmd $ Cmd "echo" :$ task.targets :$ "->" :$ task.sources
  return True

o = CC.compile [fs|example/hello.o|] [fs|example/hello.c|]
prog = CC.link [fs|example/hello|] [fs|example/hello.o|]

rules = r <> prule <> o <> prog
g = rules.graph
t = rules.tasks

order = DepGraph.buildOrder rules [fs|example/hello|]

main :: IO ()
main = do
  writeFile "graph.dot" (exportViaShow g)
  print t
  print order
  result <- Taskmaster.build g env order
  print result
