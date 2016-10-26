{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified Data.GraphQL.AST as AST
import GraphQL.API ((:>), runQuery, GraphQLValue, Server)
import GraphQL.Output (Response(..))
import GraphQL.Value (fieldSetToMap, makeField, singleton, ToValue(..))

main :: IO ()
main = defaultMain =<< tests


newtype Foo = Foo Text deriving (Eq, Show)
instance ToValue Foo where
  toValue (Foo f) = toValue f

type API = "foo" :> GraphQLValue Foo

handler :: Server API
handler = pure (Foo "qux")

tests :: IO TestTree
tests = testSpec "GraphQL API" $ do
  describe "GraphQLValue Foo" $
    it "Happy cases happily" $ do
      let input = []
      -- XXX: This test won't compile because the example is invalid: the
      -- top-level graphql value *must* be an object.
      result <- runQuery (Proxy :: Proxy (GraphQLValue Foo)) handler input
      result `shouldBe` toValue ("qux" :: Text)
  describe "\"foo\" :> GraphQLValue Foo" $
    it "Happy cases happily" $ do
      let input = [AST.SelectionField (AST.Field "bar" "foo" [] [] []) ]
      result <- runQuery (Proxy :: Proxy API) handler input
      result `shouldBe` Success (fieldSetToMap (singleton (makeField ("bar" :: Text) (Foo "qux"))))
