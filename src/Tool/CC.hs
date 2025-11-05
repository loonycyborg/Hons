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
  envVar @("cc" :. "cccom")     "gcc"            :+:
  envVar @("cc" :. "cflags")    ([] :: [String]) :+:
  envVar @("cc" :. "cpppath")   ([] :: [String]) :+:
  envVar @("cc" :. "linkcom")   "gcc"            :+:
  envVar @("cc" :. "linkflags") ([] :: [String]) :+:
  envVar @("cc" :. "libpath")   ([] :: [String]) :+:
  EnvNihil

$genToolVars

data Flag where
    Literal :: Argument a => a -> Flag
    Compile :: Flag
    Output  :: Argument a => a -> Flag
    CPPPath :: Argument a => a -> Flag
    LibPath :: Argument a => a -> Flag

instance Argument Flag where
    toCmdLine (Literal as) = toCmdLine as
    toCmdLine Compile = [encodeArg "-c"]
    toCmdLine (Output a) = encodeArg "-o" : toCmdLine a
    toCmdLine (CPPPath as) = (encodeArg "-I"<>) <$> toCmdLine as
    toCmdLine (LibPath as) = (encodeArg "-L"<>) <$> toCmdLine as

compile :: (UseEnv ToolVars vars) => Node -> Node -> RuleSet vars
compile = osCommand $ Cmd cccom :$ Literal cflags :$ CPPPath cpppath :$ Compile :$ Output substT :$ substS

link :: (UseEnv ToolVars vars, Argument s, NodeList s) => Node -> s -> RuleSet vars
link = osCommand $ Cmd linkcom :$ Literal linkflags :$ LibPath libpath :$ Output substT :$ substS
