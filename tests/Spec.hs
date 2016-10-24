{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.GraphQL.AST as AST
import GraphQL.API ((:>), runQuery, GetJSON, Server)

main :: IO ()
main = defaultMain =<< tests


newtype Foo = Foo Text deriving (Eq, Show, ToJSON)
type API = "foo" :> GetJSON Foo

handler :: Server API
handler = pure (Foo "qux")

tests :: IO TestTree
tests = testSpec "GraphQL API" $ do
  describe "GetJSON Foo" $
    it "Happy cases happily" $ do
      let input = []
      let expected = toJSON (Foo "qux")
      result <- runQuery (Proxy :: Proxy (GetJSON Foo)) handler input
      result `shouldBe` expected
  describe "\"foo\" :> GetJSON Foo" $
    it "Happy cases happily" $ do
      let input = [AST.SelectionField (AST.Field "bar" "foo" [] [] []) ]
      let expected = object ["bar" .= Foo "qux"]
      result <- runQuery (Proxy :: Proxy API) handler input
      result `shouldBe` expected
