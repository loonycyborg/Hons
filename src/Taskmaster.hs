{-# LANGUAGE GADTs, OverloadedRecordDot, InstanceSigs #-}
module Taskmaster where
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Data.Typeable

import Algebra.Graph.AdjacencyMap

import Node
import Environment

data Task where
    Task :: { targets :: [Node], sources :: [Node], action :: IO Bool } -> Task

instance Eq Task where
    (==) :: Task -> Task -> Bool
    (==) t1 t2 = head t1.targets == head t2.targets

instance Show Task where
    show (Task targets _ _) = "[[[" ++ (show . head $ targets) ++ "]]]"

transformWithNode :: Typeable vars => Node -> Environment vars -> Environment vars
transformWithNode (ValueNode _ _ tr) = eTransform tr
transformWithNode (FsNode _) = eTransform EIdentity

taskContext :: Typeable vars => AdjacencyMap Node -> HM.HashMap Node Task -> Environment vars -> Task -> Environment vars
taskContext deps taskList env task =
    let
        ts = HS.fromList task.targets
        sourceSet = flip postSet deps
        sources = foldr1 S.union $ HS.map sourceSet ts
        envs = map (nodeContext deps taskList env) (S.toList sources)
    in
        foldr1 eMerge envs

nodeContext :: Typeable vars => AdjacencyMap Node -> HM.HashMap Node Task -> Environment vars -> Node -> Environment vars
nodeContext deps taskList env node =
    let
        sources = postSet node deps
        srcEnv src = maybe (nodeContext deps taskList env src) (taskContext deps taskList env) (HM.lookup src taskList)
        envs = map srcEnv (S.toList sources)
    in
        transformWithNode node $ foldr eMerge env envs

--build taskList deps env = 