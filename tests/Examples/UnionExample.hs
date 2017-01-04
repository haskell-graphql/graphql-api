{-# LANGUAGE DataKinds #-}
module Examples.UnionExample  where

-- TODO: union code is totally wrong because I misunderstood the
-- spec. The server decides the return type and we'll probably need an
-- open sum type for returning.


import Protolude hiding (Enum, U1)
import qualified GraphQL.Internal.Validation as Validation
import GraphQL.API
import GraphQL (compileQuery, getOperation)
import GraphQL.Resolver
import GraphQL.Value (Value)

type O1 = Object "O1" '[] '[Field "o1" Text]
type O2 = Object "O2" '[] '[Field "o2" Int32]

type U1 = Union "U1" '[O1, O2]

o1 :: Handler IO O1
o1 = pure (pure "hello from O1")

o2 :: Handler IO O2
o2 = pure (pure 32)

u1 :: Handler IO U1
u1 = unionValue @O2 o2

exampleQuery :: IO (Result Value)
exampleQuery = buildResolver @IO @U1 u1 (query "{ ... on O1 { o1 } ... on O2 { o2 } }")

query :: Text -> Validation.SelectionSet
query q =
  let Right doc = compileQuery q
      Just x = getOperation doc Nothing
  in x
