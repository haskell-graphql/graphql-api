{-# LANGUAGE TypeApplications #-}
module ValueTests (tests) where

import Protolude

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe, shouldSatisfy)

import GraphQL.Value
  ( Object(..)
  , ObjectField(..)
  , unionObjects
  , objectFromList
  , toValue
  )

tests :: IO TestTree
tests = testSpec "Value" $ do
  describe "unionObject" $ do
    it "returns empty on empty list" $ do
      unionObjects [] `shouldBe` objectFromList []
    it "merges objects" $ do
      let (Just foo) = objectFromList [("foo", toValue @Int32 1),("bar",toValue @Int32 2)]
      let (Just bar) = objectFromList [("bar", toValue @Text "cow"),("baz",toValue @Int32 3)]
      let observed = unionObjects [foo, bar]
      observed `shouldBe` Nothing
    it "merges objects with unique keys" $ do
      let (Just foo) = objectFromList [("foo", toValue @Int32 1)]
      let (Just bar) = objectFromList [("bar", toValue @Text "cow"),("baz",toValue @Int32 3)]
      let (Just expected) = objectFromList [ ("foo", toValue @Int32 1)
                                           , ("bar", toValue @Text "cow")
                                           , ("baz", toValue @Int32 3)
                                           ]
      let (Just observed) = unionObjects [foo, bar]
      observed `shouldBe` expected
      expected `shouldSatisfy` prop_fieldsUnique


-- | All of the fields in an object should have unique names.
prop_fieldsUnique :: Object -> Bool
prop_fieldsUnique object =
  fieldNames == ordNub fieldNames
  where
    fieldNames = [name | ObjectField name _ <- objectFields object]
