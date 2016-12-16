-- | Tests for query validation.
module ValidationTests (tests) where

import Protolude

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified Data.GraphQL.AST as AST
import GraphQL.Validation (ValidationError(..), getErrors)

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
