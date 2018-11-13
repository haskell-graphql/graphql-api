{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for AST, including parser and encoder.
module ASTTests (tests) where

import Protolude

import Data.Attoparsec.Text (parseOnly)
import Text.RawString.QQ (r)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, forAll, resize)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)
import qualified Data.String                 as String
import GraphQL.Value (String(..))
import GraphQL.Internal.Name (Name)
import qualified GraphQL.Internal.Syntax.AST as AST
import qualified GraphQL.Internal.Syntax.Parser as Parser
import qualified GraphQL.Internal.Syntax.Encoder as Encoder

kitchenSink :: Text
kitchenSink = "query queryName($foo:ComplexType,$site:Site=MOBILE){whoever123is:node(id:[123,456]){id,... on User@defer{field2{id,alias:field1(first:10,after:$foo)@include(if:$foo){id,...frag}}}}}mutation likeStory{like(story:123)@defer{story{id}}}fragment frag on Friend{foo(size:$size,bar:$b,obj:{key:\"value\"})}\n"

dog :: Name
dog = "dog"

someName :: Name
someName = "name"

noPos :: Either a AST.Value -> Either a AST.Value
noPos (Left v) = Left v
noPos (Right v) = Right (AST.setPos (const Nothing) v)


tests :: IO TestTree
tests = testSpec "AST" $ do
  describe "Parser and encoder" $ do
    it "roundtrips on minified documents" $ do
      let actual = Encoder.queryDocument <$> parseOnly Parser.queryDocument kitchenSink
      actual `shouldBe` Right kitchenSink
    describe "parsing numbers" $ do
      it "works for some integers" $ do
        parseOnly Parser.value "1" `shouldBe` Right (AST.ValueInt 1 $ Just(0,1))
      prop "works for all integers" $ do
        \x -> parseOnly Parser.value (show x) == Right (AST.ValueInt x (Just (0,  length (show x :: String.String))))
      it "works for some floats" $ do
        parseOnly Parser.value "1.5" `shouldBe` Right (AST.ValueFloat 1.5 $ Just(0,3))
      it "treats floats as floats even if they end with .0" $ do
        parseOnly Parser.value "0.0" `shouldBe` Right (AST.ValueFloat 0.0 $ Just(0,3))
      prop "works for floats" $ do
        \x -> parseOnly Parser.value (show x) == Right (AST.ValueFloat x (Just (0,  length (show x :: String.String))))
    describe "strings" $ do
      prop "works for all strings" $ do
        \(String x) ->
          let input = AST.ValueString (AST.StringValue x) Nothing
              output = Encoder.value input in
          noPos (parseOnly Parser.value output) == Right input
      it "handles unusual strings" $ do
        let input = AST.ValueString (AST.StringValue "\fh\244") (Just (0,10))
        let output = Encoder.value input
        -- \f is \u000c
        output `shouldBe` "\"\\u000ch\244\""
        parseOnly Parser.value output `shouldBe` Right input
    describe "parsing values" $ do
      prop "works for all literal values" $ do
        forAll (resize 3 arbitrary) $ \x -> noPos (parseOnly Parser.value (Encoder.value x)) `shouldBe` Right x
      it "parses ununusual objects" $ do
        let input = AST.ValueObject
                    (AST.ObjectValue
                     [ AST.ObjectField "s"
                       $ AST.ValueString (AST.StringValue "\224\225v^6{FPDk\DC3\a")  (Just (3,28)),
                       AST.ObjectField "Hsr" (AST.ValueInt 0 (Just (32,33)) )
                     ]) (Just (0,34))
        let output = Encoder.value input
        parseOnly Parser.value output `shouldBe` Right input
      it "parses lists of floats" $ do
        let input = AST.ValueList
                      (AST.ListValue
                       [ AST.ValueFloat 1.5 $ Just (1,5)
                       , AST.ValueFloat 1.5 $ Just (5,8)
                       ])  (Just (0,9))
        let output = Encoder.value input
        output `shouldBe` "[1.5,1.5]"
        parseOnly Parser.value output `shouldBe` Right input
  describe "Parser" $ do
    it "parses shorthand syntax documents" $ do
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
                             [ AST.SelectionField (AST.Field Nothing someName [] [] []) (Just (56,84))
                             ])
                           (Just (25,107))
                         ] $ Just (0, 108)
                       ) $ Just (0, 108)
                     ] (Just (0, 108))
      parsed `shouldBe` expected

    it "parses anonymous query documents" $ do
      let query = [r|query {
                       dog {
                         name
                       }
                     }|]
      let Right parsed = parseOnly Parser.queryDocument query
      let expected = AST.QueryDocument
                     [ AST.DefinitionOperation
                       (AST.Query
                         (AST.Node Nothing [] []
                           [ AST.SelectionField
                             (AST.Field Nothing dog [] []
                               [ AST.SelectionField (AST.Field Nothing someName [] [] []) (Just (62,90))
                               ])
                             (Just (31,113))
                           ] $ Just (6, 114)) $ Just (0, 114)
                       ) $ Just (0, 114)
                     ] (Just (0, 114))
      parsed `shouldBe` expected

    it "errors on missing selection set" $ do
      let query = [r|query {
                       dog {
                         
                       }
                     }|]
      let Left parsed = parseOnly Parser.queryDocument query
      -- this is not very explicit
      parsed `shouldBe` "query document error! > definition error!: string"

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
                             [ AST.SelectionField (AST.Field Nothing someName [] [] []) (Just (56,84))
                             ])
                           (Just (25,107))
                         ] $ Just (0,131)
                       ) $ Just (0,131)
                     , AST.DefinitionOperation
                       (AST.Query
                        (AST.Node (pure "getName") [] []
                         [ AST.SelectionField
                           (AST.Field Nothing dog [] []
                            [ AST.SelectionField
                              (AST.Field Nothing "owner" [] []
                               [ AST.SelectionField (AST.Field Nothing someName [] [] []) (Just (236,266))
                               ])
                              (Just (201,291))
                            ])
                           (Just (170,314))
                         ] $ Just (137,315)) $ Just (131,315)
                       ) $ Just (131,315)
                     ] (Just (0, 315))
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
                           (AST.Node (pure "houseTrainedQuery")
                            [ AST.VariableDefinition
                                (AST.Variable "atOtherHomes")
                                (AST.TypeNamed (AST.NamedType "Boolean"))
                                (Just (AST.ValueBoolean True $Just ( 70 , 74)))
                                (Just (45,74))
                            ] []
                            [ AST.SelectionField
                                (AST.Field Nothing dog [] []
                                 [ AST.SelectionField
                                     (AST.Field Nothing "isHousetrained"
                                      [ AST.Argument "atOtherHomes"
                                          (AST.ValueVariable (AST.Variable "atOtherHomes") (Just (159 , 172)))
                                      ] [] [])
                                     (Just (130,196))
                                 ])
                                 (Just (100,218))
                            ] $ Just (27, 240)
                           ) $ Just (21, 240)
                         ) $ Just (21, 240)
                     ] (Just (21, 240))
      parsed `shouldBe` expected

    it "parses anonymous query with variables" $ do
      let query = [r|
                    query ($atOtherHomes: Boolean = true) {
                      dog {
                        isHousetrained(atOtherHomes: $atOtherHomes)
                      }
                    }
                    |]
      let Right parsed = parseOnly Parser.queryDocument query
      let expected = AST.QueryDocument
                     [ AST.DefinitionOperation
                         (AST.Query
                           (AST.Node Nothing
                            [ AST.VariableDefinition
                                (AST.Variable "atOtherHomes")
                                (AST.TypeNamed (AST.NamedType "Boolean"))
                                (Just (AST.ValueBoolean True $Just (53, 57)))
                                (Just (28,57))
                            ] []
                            [ AST.SelectionField
                                (AST.Field Nothing dog [] []
                                 [ AST.SelectionField
                                     (AST.Field Nothing "isHousetrained"
                                      [ AST.Argument "atOtherHomes"
                                          (AST.ValueVariable (AST.Variable "atOtherHomes") $Just (142, 155))
                                      ] [] [])
                                     (Just (113,179))
                                 ])
                                (Just (83,201))
                            ] $ Just (27, 223)
                           ) $ Just (21, 223)
                         ) $ Just (21, 223)
                     ] (Just (21, 223))
      parsed `shouldBe` expected
    it "parses anonymous query with variable annotation" $ do
      let query = [r|
                    query ($atOtherHomes: [Home!]) {
                      dog {
                        isHousetrained(atOtherHomes: $atOtherHomes)
                      }
                    }
                    |]
      let Right parsed = parseOnly Parser.queryDocument query
      let expected = AST.QueryDocument
                     [ AST.DefinitionOperation
                         (AST.Query
                           (AST.Node Nothing
                            [ AST.VariableDefinition
                                (AST.Variable "atOtherHomes")
                                (AST.TypeList 
                                  (AST.ListType 
                                    (AST.TypeNonNull
                                      (AST.NonNullTypeNamed (AST.NamedType "Home"))
                                    )
                                  )
                                )
                                Nothing
                                (Just (28,50))
                            ] []
                            [ AST.SelectionField
                                (AST.Field Nothing dog [] []
                                 [ AST.SelectionField
                                     (AST.Field Nothing "isHousetrained"
                                      [ AST.Argument "atOtherHomes"
                                          (AST.ValueVariable (AST.Variable "atOtherHomes") $Just (135, 148))
                                      ] [] [])
                                     (Just (106,172))
                                 ])
                                (Just (76,194))
                            ] $ Just (27, 216)
                           ) $ Just (21, 216)
                         ) $ Just (21, 216)
                     ] (Just (21, 216))
      parsed `shouldBe` expected
    it "parses anonymous query with inline argument (List, Object, Enum, String, Number)" $ do
      -- keys are not quoted for inline objects
      let query = [r|
                    query {
                      dog {
                        isHousetrained(atOtherHomes: [{testKey: 123, anotherKey: "string"}])
                      }
                    }
                    |]
      let Right parsed = parseOnly Parser.queryDocument query
      let expected = AST.QueryDocument
                     [ AST.DefinitionOperation
                         (AST.Query
                           (AST.Node Nothing
                            [] []
                            [ AST.SelectionField
                                (AST.Field Nothing dog [] []
                                 [ AST.SelectionField
                                     (AST.Field Nothing "isHousetrained"
                                      [ AST.Argument "atOtherHomes"
                                          (AST.ValueList (AST.ListValue [
                                            AST.ValueObject (AST.ObjectValue [
                                              AST.ObjectField "testKey" (AST.ValueInt 123  (Just (121, 126))),
                                              AST.ObjectField "anotherKey" (AST.ValueString (AST.StringValue "string")  (Just (138, 146)))
                                            ]) $ Just (111, 147)
                                          ])  (Just (110, 148)))
                                      ] [] [])
                                      (Just (81,172))
                                 ])
                                (Just (51,194))
                            ] $ Just (27, 216)
                           ) $ Just (21, 216)
                         ) $ Just (21,216)
                     ] (Just (21, 216))
      parsed `shouldBe` expected
    it "parses anonymous query with fragment" $ do
      -- keys are not quoted for inline objects
      let query = [r|
                    fragment dogTest on Dog {
                      name
                    }
                    query {
                      dog {
                        ...dogTest
                      }
                    }
                    |]
      let Right parsed = parseOnly Parser.queryDocument query
      let expected = AST.QueryDocument
                     [AST.DefinitionFragment (AST.FragmentDefinition "dogTest"
                        (AST.NamedType "Dog") [] [
                          AST.SelectionField (AST.Field Nothing "name" [] [] []) (Just (69,94))
                        ]) $ Just (21, 116),
                        AST.DefinitionOperation
                         (AST.Query
                           (AST.Node Nothing
                            [] []
                            [AST.SelectionField
                              (AST.Field Nothing dog [] []
                                [AST.SelectionFragmentSpread (AST.FragmentSpread "dogTest" []) (Just (176,209))
                                ])
                              (Just (146,231))
                            ] $ Just (122, 253)
                           ) $ Just (116, 253)
                         ) $ Just (116, 253)
                     ] (Just (21, 253))
      parsed `shouldBe` expected
