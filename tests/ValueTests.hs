{-# LANGUAGE DeriveGeneric #-}
module ValueTests (tests) where

import Protolude

import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe, shouldSatisfy)

import qualified GraphQL.Internal.Syntax.AST as AST
import GraphQL.Internal.Arbitrary (arbitraryText, arbitraryNonEmpty)
import GraphQL.Value
  ( Object
  , Value'(ValueObject')
  , ObjectField'(..)
  , astToVariableValue
  , unionObjects
  , objectFields
  , objectFromList
  , toValue
  )
import GraphQL.Internal.Value.FromValue (FromValue(..), prop_roundtripValue)

data Resource = Resource
    { resText     :: Text
    , resInt      :: Int32
    , resDouble   :: Double
    , resBool     :: Bool
    } deriving (Generic, Eq, Show)

instance FromValue Resource

tests :: IO TestTree
tests = testSpec "Value" $ do
  describe "unionObject" $ do
    it "returns empty on empty list" $ do
      unionObjects [] `shouldBe` (objectFromList [] :: Maybe Object)
    it "merges objects" $ do
      let (Just foo) = objectFromList [ ("foo", toValue @Int32 1)
                                      , ("bar",toValue @Int32 2)]
      let (Just bar) = objectFromList [ ("bar", toValue @Text "cow")
                                      , ("baz",toValue @Int32 3)]
      let observed = unionObjects [foo, bar]
      observed `shouldBe` Nothing
    it "merges objects with unique keys" $ do
      let (Just foo) = objectFromList [("foo", toValue @Int32 1)]
      let (Just bar) = objectFromList [ ("bar", toValue @Text "cow")
                                      , ("baz",toValue @Int32 3)]
      let (Just expected) = objectFromList [ ("foo", toValue @Int32 1)
                                           , ("bar", toValue @Text "cow")
                                           , ("baz", toValue @Int32 3)
                                           ]
      let (Just observed) = unionObjects [foo, bar]
      observed `shouldBe` expected
      expected `shouldSatisfy` prop_fieldsUnique
  describe "Objects" $ do
    prop "have unique fields" $ do
      prop_fieldsUnique
    -- See https://github.com/haskell-graphql/graphql-api/pull/178 for background
    it "derives fromValue instances for objects with more than three fields" $ do
      let Just value = objectFromList 
            [ ("resText",   toValue @Text "text")
            , ("resBool",   toValue @Bool False)
            , ("resDouble", toValue @Double 1.2)
            , ("resInt",    toValue @Int32 32)
            ]
      let Right observed = fromValue $ ValueObject' value
      let expected = Resource
            { resText   = "text"
            , resInt    = 32
            , resDouble = 1.2
            , resBool   = False 
            }
      observed `shouldBe` expected
      
  describe "ToValue / FromValue instances" $ do
    prop "Bool" $ prop_roundtripValue @Bool
    prop "Int32" $ prop_roundtripValue @Int32
    prop "Double" $ prop_roundtripValue @Double
    prop "Text" $ forAll arbitraryText prop_roundtripValue
    prop "Lists" $ prop_roundtripValue @[Int32]
    prop "Non-empty lists" $ forAll (arbitraryNonEmpty @Int32) prop_roundtripValue
  describe "AST" $ do
    it "Objects converted from AST have unique fields" $ do
      let input = AST.ObjectValue [ AST.ObjectField "foo" (AST.ValueString (AST.StringValue "bar"))
                                  , AST.ObjectField "foo" (AST.ValueString (AST.StringValue "qux"))
                                  ]
      astToVariableValue (AST.ValueObject input) `shouldBe` Nothing


-- | All of the fields in an object should have unique names.
prop_fieldsUnique :: Object -> Bool
prop_fieldsUnique object =
  fieldNames == ordNub fieldNames
  where
    fieldNames = [name | ObjectField name _ <- objectFields object]
