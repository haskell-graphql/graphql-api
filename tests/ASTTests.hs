{-# LANGUAGE QuasiQuotes #-}

-- | Tests for AST, including parser and encoder.
module ASTTests (tests) where

import Protolude

import Data.Attoparsec.Text (parseOnly)
import Text.RawString.QQ (r)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, forAll, resize)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.Value (String(..))
import qualified GraphQL.Internal.AST as AST
import qualified GraphQL.Internal.Parser as Parser
import qualified GraphQL.Internal.Encoder as Encoder

kitchenSink :: Text
kitchenSink = "query queryName($foo:ComplexType,$site:Site=MOBILE){whoever123is:node(id:[123,456]){id,... on User@defer{field2{id,alias:field1(first:10,after:$foo)@include(if:$foo){id,...frag}}}}}mutation likeStory{like(story:123)@defer{story{id}}}fragment frag on Friend{foo(size:$size,bar:$b,obj:{key:\"value\"})}\n"

dog :: AST.Name
dog = AST.unsafeMakeName "dog"

someName :: AST.Name
someName = AST.unsafeMakeName "name"

tests :: IO TestTree
tests = testSpec "AST" $ do
  describe "Parser and encoder" $ do
    it "roundtrips on minified documents" $ do
      let actual = Encoder.queryDocument <$> parseOnly Parser.queryDocument kitchenSink
      actual `shouldBe` Right kitchenSink
    describe "parsing numbers" $ do
      it "works for some integers" $ do
        parseOnly Parser.value "1" `shouldBe` Right (AST.ValueInt 1)
      prop "works for all integers" $ do
        \x -> parseOnly Parser.value (show x) == Right (AST.ValueInt x)
      it "works for some floats" $ do
        parseOnly Parser.value "1.5" `shouldBe` Right (AST.ValueFloat 1.5)
      it "treats floats as floats even if they end with .0" $ do
        parseOnly Parser.value "0.0" `shouldBe` Right (AST.ValueFloat 0.0)
      prop "works for floats" $ do
        \x -> parseOnly Parser.value (show x) == Right (AST.ValueFloat x)
    describe "strings" $ do
      prop "works for all strings" $ do
        \(String x) ->
          let input = AST.ValueString (AST.StringValue x)
              output = Encoder.value input in
          parseOnly Parser.value output == Right input
      it "handles unusual strings" $ do
        let input = AST.ValueString (AST.StringValue "\fh\244")
        let output = Encoder.value input
        -- \f is \u000c
        output `shouldBe` "\"\\u000ch\244\""
        parseOnly Parser.value output `shouldBe` Right input
    describe "parsing values" $ do
      prop "works for all literal values" $ do
        forAll (resize 3 arbitrary) $ \x -> parseOnly Parser.value (Encoder.value x) `shouldBe` Right x
      it "parses ununusual objects" $ do
        let input = AST.ValueObject
                    (AST.ObjectValue
                     [ AST.ObjectField (AST.unsafeMakeName "s")
                       (AST.ValueString (AST.StringValue "\224\225v^6{FPDk\DC3\a")),
                       AST.ObjectField (AST.unsafeMakeName "Hsr") (AST.ValueInt 0)
                     ])
        let output = Encoder.value input
        parseOnly Parser.value output `shouldBe` Right input
      it "parses lists of floats" $ do
        let input = AST.ValueList
                      (AST.ListValue
                       [ AST.ValueFloat 1.5
                       , AST.ValueFloat 1.5
                       ])
        let output = Encoder.value input
        output `shouldBe` "[1.5,1.5]"
        parseOnly Parser.value output `shouldBe` Right input
  describe "Parser" $ do
    it "parses anonymous query documents" $ do
      let query = [r|{
                       dog {
                         name
                       }
                     }|]
      let Right parsed = parseOnly Parser.queryDocument query
      let expected = AST.QueryDocument
                     [ AST.DefinitionOperation
                       (AST.AnonymousQuery
                         [ AST.SelectionField
                           (AST.Field Nothing dog [] []
                             [ AST.SelectionField (AST.Field Nothing someName [] [] [])
                             ])
                         ])
                     ]
      parsed `shouldBe` expected

    it "parses invalid documents" $ do
      let query = [r|{
                       dog {
                         name
                       }
                     }

                     query getName {
                       dog {
                         owner {
                           name
                         }
                       }
                     }|]
      let Right parsed = parseOnly Parser.queryDocument query
      let expected = AST.QueryDocument
                     [ AST.DefinitionOperation
                       (AST.AnonymousQuery
                         [ AST.SelectionField
                           (AST.Field Nothing dog [] []
                             [ AST.SelectionField (AST.Field Nothing someName [] [] [])
                             ])
                         ])
                     , AST.DefinitionOperation
                       (AST.Query
                        (AST.Node (AST.unsafeMakeName "getName") [] []
                         [ AST.SelectionField
                           (AST.Field Nothing dog [] []
                            [ AST.SelectionField
                              (AST.Field Nothing (AST.unsafeMakeName "owner") [] []
                               [ AST.SelectionField (AST.Field Nothing someName [] [] [])
                               ])
                            ])
                         ]))
                     ]
      parsed `shouldBe` expected

    it "includes variable definitions" $ do
      let query = [r|
                    query houseTrainedQuery($atOtherHomes: Boolean = true) {
                      dog {
                        isHousetrained(atOtherHomes: $atOtherHomes)
                      }
                    }
                    |]
      let Right parsed = parseOnly Parser.queryDocument query
      let expected = AST.QueryDocument
                     [ AST.DefinitionOperation
                         (AST.Query
                           (AST.Node (AST.unsafeMakeName "houseTrainedQuery")
                            [ AST.VariableDefinition
                                (AST.Variable (AST.unsafeMakeName "atOtherHomes"))
                                (AST.TypeNamed (AST.NamedType (AST.unsafeMakeName "Boolean")))
                                (Just (AST.ValueBoolean True))
                            ] []
                            [ AST.SelectionField
                                (AST.Field Nothing dog [] []
                                 [ AST.SelectionField
                                     (AST.Field Nothing (AST.unsafeMakeName "isHousetrained")
                                      [ AST.Argument (AST.unsafeMakeName "atOtherHomes")
                                          (AST.ValueVariable (AST.Variable (AST.unsafeMakeName "atOtherHomes")))
                                      ] [] [])
                                 ])
                            ]))
                     ]
      parsed `shouldBe` expected
