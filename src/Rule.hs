{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Rule where
import System.OsPath

import Node

data Rule where
    Rule :: { targets :: [Node], sources :: [Node], action :: IO Bool } -> Rule
    RuleChain :: { targetRule :: Rule, sourceRules :: [Rule] } -> Rule
    Depends :: { target :: Node, source :: Node } -> Rule

instance Show Rule where
    show :: Rule -> String
    show (Rule targets sources _) = show targets ++ "->" ++ show sources