{-# LANGUAGE GADTs, FlexibleContexts, BlockArguments, TypeApplications, DataKinds, ConstraintKinds #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Tool.CC where

import Builder
import CmdLine
import Taskmaster
import Environment
import Node
import DepGraph
import Data.Tagged

type ToolVars = [Tagged "cc.com" String, Tagged "cc.linkcom" String]
toolEnv :: EnvProto ToolVars
toolEnv =
  envVar "gcc" :+:
  envVar "gcc" :+:
  EnvNihil

data Flag where
    Compile :: Flag
    Output  :: Argument a => a -> Flag

instance Argument Flag where
    toCmdLine Compile = [encodeArg "-c"]
    toCmdLine (Output a) = encodeArg "-o" : toCmdLine a

compile :: (UseEnv ToolVars vars) => Node -> Node -> RuleSet vars
compile target source = command target source do
    env <- getenv
    let cc = eLookup @"cc.com" env
    fmap success $ liftIO $ spawnCmdPrint $ Cmd cc :$ Compile :$ Output target :$ source

link :: (UseEnv ToolVars vars, Argument s, NodeList s) => Node -> s -> RuleSet vars
link target sources = command target sources do
    env <- getenv
    let ld = eLookup @"cc.linkcom" env
    fmap success $ liftIO $ spawnCmdPrint $ Cmd ld :$ Output target :$ sources
