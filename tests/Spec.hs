{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified Data.GraphQL.AST as AST
import GraphQL.Validation (ValidationError(..), getErrors)
import qualified TypeTests
import qualified TypeApiTests

-- import examples to ensure they compile
import Examples.UnionExample ()

main :: IO ()
main = do
  t <- sequence [tests, TypeTests.typeTests, TypeApiTests.typeApiTests]
  defaultMain (testGroup "spec" t)

tests :: IO TestTree
tests = testSpec "GraphQL API" $ do
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
