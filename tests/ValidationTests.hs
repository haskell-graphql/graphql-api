{-# LANGUAGE TypeApplications #-}

-- | Tests for query validation.
module ValidationTests (tests) where

import Protolude

import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.Internal.Name (Name(Name), mempty)
import qualified GraphQL.Internal.Syntax.AST as AST
import GraphQL.Internal.Schema (Schema)
import GraphQL.Internal.Validation
  ( ValidationError(..)
  , findDuplicates
  , getErrors
  )

me :: Maybe Name
me = pure "me"

someName :: Name
someName = "name"

dog :: Name
dog = "dog"

-- | Schema used for these tests. Since none of them do type-level stuff, we
-- don't need to define it.
schema :: Schema
schema = undefined

tests :: IO TestTree
tests = testSpec "Validation" $ do
  describe "getErrors" $ do
    it "Treats simple queries as valid" $ do
      let doc = AST.QueryDocument
                [ AST.DefinitionOperation
                  ( AST.Query
                    ( AST.Node me [] []
                      [ AST.SelectionField (AST.Field Nothing someName [] [] [])
                      ]
                    )
                  )
                ]
      getErrors schema doc `shouldBe` []

    it "Treats anonymous queries as valid" $ do
      let doc = AST.QueryDocument
                [ AST.DefinitionOperation
                  (AST.Query
                    (AST.Node (Name mempty) [] []
                      [ AST.SelectionField
                        (AST.Field Nothing dog [] []
                          [ AST.SelectionField (AST.Field Nothing someName [] [] [])
                          ])
                      ]))
                ]
      getErrors schema doc `shouldBe` []

    it "Treats anonymous queries with variables as valid" $ do
      let doc = AST.QueryDocument
                [ AST.DefinitionOperation
                    (AST.Query
                      (AST.Node (Name mempty)
                       [ AST.VariableDefinition
                           (AST.Variable "atOtherHomes")
                           (AST.TypeNamed (AST.NamedType "Boolean"))
                           (Just (AST.ValueBoolean True))
                       ] []
                       [ AST.SelectionField
                           (AST.Field Nothing dog [] []
                            [ AST.SelectionField
                                (AST.Field Nothing "isHousetrained"
                                 [ AST.Argument "atOtherHomes"
                                     (AST.ValueVariable (AST.Variable "atOtherHomes"))
                                 ] [] [])
                            ])
                       ]))
                ]
      getErrors schema doc `shouldBe` []

    it "Detects duplicate operation names" $ do
      let doc = AST.QueryDocument
                [ AST.DefinitionOperation
                  ( AST.Query
                    ( AST.Node me [] []
                      [ AST.SelectionField (AST.Field Nothing someName [] [] [])
                      ]
                    )
                  )
                , AST.DefinitionOperation
                  ( AST.Query
                    ( AST.Node me [] []
                      [ AST.SelectionField (AST.Field Nothing someName [] [] [])
                      ]
                    )
                  )
                ]
      getErrors schema doc `shouldBe` [DuplicateOperation me]

    it "Detects duplicate anonymous operations" $ do
      let doc = AST.QueryDocument
                [ AST.DefinitionOperation
                  ( AST.AnonymousQuery
                    [ AST.SelectionField (AST.Field Nothing someName [] [] [])
                    ]
                  )
                , AST.DefinitionOperation
                  ( AST.AnonymousQuery
                    [ AST.SelectionField (AST.Field Nothing someName [] [] [])
                    ]
                  )
                ]
      getErrors schema doc `shouldBe` [MixedAnonymousOperations 2 []]

  describe "findDuplicates" $ do
    prop "returns empty on unique lists" $ do
      \xs -> findDuplicates @Int (ordNub xs) === []
    prop "finds only duplicates" $ \xs -> do
      all (>1) (count xs <$> findDuplicates @Int xs)
    prop "finds all duplicates" $ \xs -> do
      (sort . findDuplicates @Int) xs === (ordNub . sort . filter ((> 1) . count xs)) xs


-- | Count the number of times 'x' occurs in 'xs'.
count :: Eq a => [a] -> a -> Int
count xs x = (length . filter (== x)) xs
