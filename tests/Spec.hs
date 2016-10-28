{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified Data.Map as Map
import qualified Data.GraphQL.AST as AST
import GraphQL.API ((:>), runQuery, GraphQLValue, Server)
import GraphQL.Output (Response(..))
import GraphQL.Validation (validate)
import GraphQL.Value (fieldSetToMap, makeField, singleton, ToValue(..))

main :: IO ()
main = defaultMain =<< tests


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

  describe "Validation" $
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
      isJust (validate doc) `shouldBe` True
