module ValueTests (tests) where

import Protolude

import Test.Hspec.QuickCheck (prop)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe, shouldSatisfy)

import GraphQL.Internal.AST (unsafeMakeName)
import GraphQL.Value
  ( Object(..)
  , ObjectField(..)
  , unionObjects
  , objectFromList
  , prop_roundtripFromValue
  , toValue
  )

tests :: IO TestTree
tests = testSpec "Value" $ do
  describe "unionObject" $ do
    it "returns empty on empty list" $ do
      unionObjects [] `shouldBe` objectFromList []
    it "merges objects" $ do
      let (Just foo) = objectFromList [ (unsafeMakeName "foo", toValue @Int32 1)
                                      , (unsafeMakeName "bar",toValue @Int32 2)]
      let (Just bar) = objectFromList [ (unsafeMakeName "bar", toValue @Text "cow")
                                      , (unsafeMakeName "baz",toValue @Int32 3)]
      let observed = unionObjects [foo, bar]
      observed `shouldBe` Nothing
    it "merges objects with unique keys" $ do
      let (Just foo) = objectFromList [(unsafeMakeName "foo", toValue @Int32 1)]
      let (Just bar) = objectFromList [ (unsafeMakeName "bar", toValue @Text "cow")
                                      , (unsafeMakeName "baz",toValue @Int32 3)]
      let (Just expected) = objectFromList [ (unsafeMakeName "foo", toValue @Int32 1)
                                           , (unsafeMakeName "bar", toValue @Text "cow")
                                           , (unsafeMakeName "baz", toValue @Int32 3)
                                           ]
      let (Just observed) = unionObjects [foo, bar]
      observed `shouldBe` expected
      expected `shouldSatisfy` prop_fieldsUnique
  describe "Objects" $ do
    prop "have unique fields" $ do
      prop_fieldsUnique
  describe "AST" $ do
    prop "Values can be converted to AST and back" $ do
      prop_roundtripFromValue


-- | All of the fields in an object should have unique names.
prop_fieldsUnique :: Object -> Bool
prop_fieldsUnique object =
  fieldNames == ordNub fieldNames
  where
    fieldNames = [name | ObjectField name _ <- objectFields object]
