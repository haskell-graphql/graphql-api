{-# LANGUAGE DataKinds #-}
module Examples.UnionExample  where

import Protolude hiding (Enum, U1)
import qualified GraphQL.Internal.Validation as Validation
import GraphQL.API
import GraphQL (compileQuery, getOperation)
import GraphQL.Resolver
import GraphQL.Value (Value)

-- Slightly reduced example from the spec
type MiniCat = Object "MiniCat" '[] '[Field "name" Text, Field "meowVolume" Int32]
type MiniDog = Object "MiniDog" '[] '[Field "barkVolume" Int32]

type CatOrDog = Union "CatOrDog" '[MiniCat, MiniDog]

miniCat :: Handler IO MiniCat
miniCat = pure (pure "Felix" :<> pure 32)

miniDog :: Handler IO MiniDog
miniDog = pure (pure 100)

catOrDog :: Handler IO CatOrDog
catOrDog = unionValue @MiniCat miniCat

exampleQuery :: IO (Result Value)
exampleQuery = buildResolver @IO @CatOrDog catOrDog (query "{ ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } }")

query :: Text -> Validation.SelectionSet
query q =
  let Right doc = compileQuery q
      Just x = getOperation doc Nothing
  in x
