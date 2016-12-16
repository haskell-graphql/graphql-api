{-# LANGUAGE TypeApplications #-}
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
    it "merges objects" $ do
      let foo = toValue (objectFromList [("foo", toValue @Int32 1),("bar",toValue @Int32 2)])
      let bar = toValue (objectFromList [("bar", toValue @Text "cow"),("baz",toValue @Int32 3)])
      -- TODO: This is the *wrong* behaviour. Documenting it in a test now to
      -- make it easier to talk about.
      let expected = [ ("foo", toValue @Int32 1)
                     , ("bar", toValue @Int32 2)
                     , ("bar", toValue @Text "cow")
                     , ("baz", toValue @Int32 3)
                     ]
      unionObject [foo,bar] `shouldBe` Right (toValue (objectFromList expected))
