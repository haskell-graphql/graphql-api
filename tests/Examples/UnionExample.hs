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
type CatOrDogList = List (Union "CatOrDog" '[MiniCat, MiniDog])

miniCat :: Text -> Handler IO MiniCat
miniCat name = pure (pure name :<> pure 32)

miniDog :: Handler IO MiniDog
miniDog = pure (pure 100)

catOrDog :: Handler IO CatOrDog
catOrDog = do
  name <- pure "Hello" -- we can do nomadic actions
  unionValue @MiniCat (miniCat name)

catOrDogList :: Handler IO CatOrDogList
catOrDogList =
  [ unionValue @MiniCat (miniCat "Felix")
  , unionValue @MiniCat (miniCat "Mini")
  , unionValue @MiniDog miniDog
  ]

exampleQuery :: IO (Result Value)
exampleQuery = buildResolver @IO @CatOrDog catOrDog (query "{ ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } }")

-- unionValue can also be used in a list:
exampleListQuery :: IO (Result Value)
exampleListQuery = buildResolver @IO @CatOrDogList catOrDogList  (query "{ ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } }")

query :: Text -> Validation.SelectionSet
query q =
  let Right doc = compileQuery q
      Just x = getOperation doc Nothing
  in x
