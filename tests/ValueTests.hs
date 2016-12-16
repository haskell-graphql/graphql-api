module ValueTests (tests) where

import Protolude

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.Value (unionObject, objectFromList, toValue)

tests :: IO TestTree
tests = testSpec "Value" $ do
  describe "unionObject" $ do
    it "returns empty on empty list" $ do
      unionObject [] `shouldBe` Right (toValue (objectFromList []))
