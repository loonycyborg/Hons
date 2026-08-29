{-# LANGUAGE DataKinds, OverloadedStrings, OverloadedLists, ImplicitParams #-}
module EnvironmentSpec (spec) where

import Test.Syd

import Environment
import Node

node :: Node
node = mkFsNodeFromString "node"

envp =
    envVar @(LocalVar "varstring") ("def" :: StrVar) :+:
    envVar @(LocalVar "varint") (42 :: Int) :+:
    envVar @(LocalVar "vartslist") ([] :: TSList StrVar) :+:
    EnvNihil

env = makeEnv envp

spec :: Spec
spec = do
    describe "Environment" $ do
        it "default values" $ do
            eLookup (LocalVar "varstring") env `shouldBe` "def"
            eLookup (LocalVar "varint") env `shouldBe` 42
            eLookup (LocalVar "vartslist") env `shouldBe` []
        it "replace" $ do
            eLookup (LocalVar "varstring") (eReplace (LocalVar "varstring") "changed" env) `shouldBe` "changed"
        it "modify" $ do
            eLookup (LocalVar "varint") (eUpdate (LocalVar "varint") (fmap Just (+1)) env) `shouldBe` 43
        it "insertTS" $ do
            let ?target = node in eLookup (LocalVar "vartslist") (eInsertTS (LocalVar "vartslist") ["a", "b"] env) `shouldBe` TSList [(node, ["a", "b"])]
