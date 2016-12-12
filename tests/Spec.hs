{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified Data.Map as Map
import qualified Data.GraphQL.AST as AST
import GraphQL.API ((:>), runQuery, GraphQLValue, Server)
import GraphQL.Output (Response(..))
import GraphQL.Validation (ValidationError(..), getErrors)
import GraphQL.Value (fieldSetToMap, makeField, singleton, ToValue(..))
import qualified TypeTests
import qualified TypeApiTests

main :: IO ()
main = do
  t <- sequence [tests, TypeTests.typeTests, TypeApiTests.typeApiTests]
  defaultMain (testGroup "spec" t)


newtype Foo = Foo Text deriving (Eq, Show)
instance ToValue Foo where
  toValue (Foo f) = toValue f

newtype BarMap = BarMap (Map Text Int32) deriving (Eq, Show)
instance ToValue BarMap where
  toValue (BarMap v) = toValue v

type API = "foo" :> GraphQLValue Foo

handler :: Server API
handler = pure (Foo "qux")

tests :: IO TestTree
tests = testSpec "GraphQL API" $ do
  describe "GraphQLValue BarMap" $
    it "Happy cases happily" $ do
      -- XXX: This should not pass, because it ought to be invalid to query
      -- without specifying leaf fields.
      let input = []
      let handler' = pure (BarMap (Map.singleton "qux" 1))
      result <- runQuery (Proxy :: Proxy (GraphQLValue BarMap)) handler' input
      result `shouldBe` Success (fieldSetToMap (singleton (makeField ("qux" :: Text) (1 :: Int32))))
  describe "\"foo\" :> GraphQLValue Foo" $
    it "Happy cases happily" $ do
      let input = [AST.SelectionField (AST.Field "bar" "foo" [] [] []) ]
      result <- runQuery (Proxy :: Proxy API) handler input
      result `shouldBe` Success (fieldSetToMap (singleton (makeField ("bar" :: Text) (Foo "qux"))))

  describe "Validation" $ do
    it "Treats simple queries as valid" $ do
      let doc = AST.Document
                [ AST.DefinitionOperation
                  ( AST.Query
                    ( AST.Node "me" [] []
                      [ AST.SelectionField (AST.Field "name" "name" [] [] [])
                      ]
                    )
                  )
                ]
      getErrors doc `shouldBe` []

    it "Detects duplicate operation names" $ do
      let doc = AST.Document
                [ AST.DefinitionOperation
                  ( AST.Query
                    ( AST.Node "me" [] []
                      [ AST.SelectionField (AST.Field "name" "name" [] [] [])
                      ]
                    )
                  )
                , AST.DefinitionOperation
                  ( AST.Query
                    ( AST.Node "me" [] []
                      [ AST.SelectionField (AST.Field "name" "name" [] [] [])
                      ]
                    )
                  )
                ]
      getErrors doc `shouldBe` [DuplicateOperation "me"]
