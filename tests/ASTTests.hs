module ASTTests (tests) where

import Protolude

import Data.Attoparsec.Text (parseOnly)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified GraphQL.Internal.Parser as Parser
import qualified GraphQL.Internal.Encoder as Encoder

kitchenSink :: Text
kitchenSink = "query queryName($foo:ComplexType,$site:Site=MOBILE){whoever123is:node(id:[123,456]){id,... on User@defer{field2{id,alias:field1(first:10,after:$foo)@include(if:$foo){id,...frag}}}}}mutation likeStory{like(story:123)@defer{story{id}}}fragment frag on Friend{foo(size:$size,bar:$b,obj:{key:\"value\"})}\n"

tests :: IO TestTree
tests = testSpec "AST" $ do
  describe "Parser and encoder" $ do
    it "roundtrips on minified documents" $ do
      let actual = Encoder.document <$> parseOnly Parser.document kitchenSink
      actual `shouldBe` Right kitchenSink
