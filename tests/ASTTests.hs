module ASTTests (tests) where

import Protolude

import Data.Attoparsec.Text (parseOnly)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, discard, forAll, verbose)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.Value (String(..), valueToAST)
import qualified GraphQL.Internal.AST as AST
import qualified GraphQL.Internal.Parser as Parser
import qualified GraphQL.Internal.Encoder as Encoder

kitchenSink :: Text
kitchenSink = "query queryName($foo:ComplexType,$site:Site=MOBILE){whoever123is:node(id:[123,456]){id,... on User@defer{field2{id,alias:field1(first:10,after:$foo)@include(if:$foo){id,...frag}}}}}mutation likeStory{like(story:123)@defer{story{id}}}fragment frag on Friend{foo(size:$size,bar:$b,obj:{key:\"value\"})}\n"

genASTValue :: Gen AST.Value
genASTValue = do
  v <- valueToAST <$> arbitrary
  maybe discard pure v

tests :: IO TestTree
tests = testSpec "AST" $ do
  describe "Parser and encoder" $ do
    it "roundtrips on minified documents" $ do
      let actual = Encoder.document <$> parseOnly Parser.document kitchenSink
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
        forAll genASTValue $ \x -> verbose $ (parseOnly Parser.value) (Encoder.value x) `shouldBe` Right x
      it "parses ununusual objects" $ do
        let input = AST.ValueObject
                    (AST.ObjectValue
                     [ AST.ObjectField "s"
                       (AST.ValueString (AST.StringValue "\224\225v^6{FPDk\DC3\a")),
                       AST.ObjectField "Hsr" (AST.ValueInt 0)
                     ])
        let output = traceShowId $ Encoder.value input
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
