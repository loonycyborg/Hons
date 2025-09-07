{-# LANGUAGE BlockArguments, DataKinds #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Tool.CC where

import CmdLine
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
compile target source = mkCmdTask @ToolVars target source $ Cmd (subst @"cc.com") :$ Compile :$ Output target :$ source

link :: (UseEnv ToolVars vars, Argument s, NodeList s) => Node -> s -> RuleSet vars
link target sources = mkCmdTask @ToolVars target sources $ Cmd (subst @"cc.linkcom") :$ Output target :$ sources
