{-# LANGUAGE GADTs, FlexibleContexts, BlockArguments, TypeApplications, DataKinds #-}
module Tool.CC where

import Builder
import CmdLine
import Taskmaster
import Environment
import Node
import DepGraph

data Flag where
    Compile :: Flag
    Output  :: Argument a => a -> Flag

instance Argument Flag where
    toCmdLine Compile = [encodeArg "-c"]
    toCmdLine (Output a) = encodeArg "-o" : toCmdLine a

compile :: (ConstructionVariable (LookupType "cc.com" vars), Argument (LookupType "cc.com" vars)) => Node -> Node -> RuleSet vars
compile target source = command target source do
    env <- getenv
    let cc = eLookup @"cc.com" env
    fmap success $ liftIO $ spawnCmdPrint $ Cmd cc :$ Compile :$ Output target :$ source

link :: (ConstructionVariable (LookupType "cc.linkcom" vars), Argument (LookupType "cc.linkcom" vars), Argument s, NodeList s) => Node -> s -> RuleSet vars
link target sources = command target sources do
    env <- getenv
    let ld = eLookup @"cc.linkcom" env
    fmap success $ liftIO $ spawnCmdPrint $ Cmd ld :$ Output target :$ sources
