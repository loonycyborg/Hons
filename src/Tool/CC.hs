{-# LANGUAGE BlockArguments, DataKinds, TemplateHaskell, ImplicitParams #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Tool.CC where

import CmdLine
import Environment
import Node
import DepGraph
import Data.Tagged

import ToolTH

toolEnv =
  envVar @("cc" :. "com")     "gcc" :+:
  envVar @("cc" :. "linkcom") "gcc" :+:
  EnvNihil

$genToolVars

data Flag where
    Compile :: Flag
    Output  :: Argument a => a -> Flag

instance Argument Flag where
    toCmdLine Compile = [encodeArg "-c"]
    toCmdLine (Output a) = encodeArg "-o" : toCmdLine a

compile :: (UseEnv ToolVars vars) => Node -> Node -> RuleSet vars
compile = osCommand $ Cmd com :$ Compile :$ Output substT :$ substS

link :: (UseEnv ToolVars vars, Argument s, NodeList s) => Node -> s -> RuleSet vars
link = osCommand $ Cmd linkcom :$ Output substT :$ substS
