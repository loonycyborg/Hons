{-# LANGUAGE BlockArguments, DataKinds, TemplateHaskell, ImplicitParams, OverloadedStrings, OverloadedLists #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Tool.CC where

import CmdLine
import Environment
import Node
import DepGraph
import Data.Tagged

import ToolTH

toolEnv =
  envVar @("cc" :. "cccom")     ("gcc" :: StrVar) :+:
  envVar @("cc" :. "cflags")    ([] :: TSList StrVar)  :+:
  envVar @("cc" :. "cpppath")   ([] :: TSList StrVar)  :+:
  envVar @("cc" :. "linkcom")   ("gcc" :: StrVar) :+:
  envVar @("cc" :. "linkflags") ([] :: TSList StrVar)  :+:
  envVar @("cc" :. "libpath")   ([] :: TSList StrVar)  :+:
  envVar @("cc" :. "libs")      ([] :: TSList Lib)     :+:
  EnvNihil

data Lib = LibSpec { name :: StrVar } | LibFile { name :: StrVar } deriving (Show, Read, Eq)
instance ConstructionVariable Lib where
  merge = (<>)
instance Value Lib where
  toCmdLine lib = toCmdLine lib.name

$genToolVars

data Flag where
    Literal :: Value a => a -> Flag
    Compile :: Flag
    Output  :: Value a => a -> Flag
    CPPPath :: Value a => a -> Flag
    LibPath :: Value a => a -> Flag
    Libs    :: (Foldable f, Value (f Lib)) => f Lib -> Flag

instance Value Flag where
    toCmdLine (Literal as) = toCmdLine as
    toCmdLine Compile = [encodeVal "-c"]
    toCmdLine (Output a) = encodeVal "-o" : toCmdLine a
    toCmdLine (CPPPath as) = (encodeVal "-I"<>) <$> toCmdLine as
    toCmdLine (LibPath as) = (encodeVal "-L"<>) <$> toCmdLine as
    toCmdLine (Libs as) = concatMap libflag as where
      libflag x = case x of
        LibSpec l -> (encodeVal "-l"<>) <$> toCmdLine l
        LibFile l -> toCmdLine l
compile :: (UseEnv ToolVars vars) => Node -> Node -> RuleSet vars
compile = osCommand $ Cmd cccom :$ Literal cflags :$ CPPPath cpppath :$ Compile :$ Output substT :$ substS

link :: (UseEnv ToolVars vars, Value s, NodeList s) => Node -> s -> RuleSet vars
link = osCommand $ Cmd linkcom :$ Literal linkflags :$ LibPath libpath :$ Libs libs :$ Output substT :$ substS
