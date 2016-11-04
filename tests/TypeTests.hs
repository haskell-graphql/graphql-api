{-# LANGUAGE TypeApplications, DataKinds #-}
module TypeTests where

import Protolude

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.MuckTom
import GraphQL.Schema

type FD1 = Field "hello" Int

typeTests :: IO TestTree
typeTests = testSpec "Type" $ do
  describe "Field" $
    it "encodes correctly" $ do
    getFieldDefinition @(Field "hello" Int) `shouldBe` (FieldDefinition (Name "hello") [] (TypeNamed (BuiltinType GInt)))
