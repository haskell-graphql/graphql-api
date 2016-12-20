{-# LANGUAGE DataKinds #-}
module Examples.UnionExample  where

import Protolude hiding (Enum)
import qualified GraphQL.Internal.AST as AST
import Data.Attoparsec.Text (parseOnly, endOfInput)
import GraphQL.Internal.Parser (document)

import GraphQL.API
import GraphQL.Server
import GraphQL.Value (Value)

type O1 = Object "O1" '[] '[Field "o1" Text]
type O2 = Object "O2" '[] '[Field "o2" Text]

type T = Union "U" '[O1, O2]

o1 :: Handler IO O1
o1 = pure (pure "hello from O1")

o2 :: Handler IO O2
o2 = pure (pure "hello from O2")

tHandler :: Handler IO T
tHandler = o1 :<|> o2

exampleQuery :: IO Value
exampleQuery = buildResolver @IO @T tHandler (query "{ ... on O1 { o1 } ... on O2 { o2 } }")

query :: Text -> AST.SelectionSet
query q =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
       parseOnly (document <* endOfInput) q
  in selectionSet
