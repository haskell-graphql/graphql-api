{-# LANGUAGE DataKinds #-}
module Examples.UnionExample  where

import Protolude hiding (Enum)
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
  name <- pure "MonadicFelix" -- we can do monadic actions
  unionValue @MiniCat (miniCat name)

catOrDogList :: Handler IO CatOrDogList
catOrDogList =
  [ unionValue @MiniCat (miniCat "Felix")
  , unionValue @MiniCat (miniCat "Mini")
  , unionValue @MiniDog miniDog
  ]

-- | Show usage of a single unionValue
-- >>> exampleQuery
-- Result [] (ValueObject (Object {objectFields = [ObjectField (Name {getNameText = "name"}) (ValueString (String "MonadicFelix")),ObjectField (Name {getNameText = "meowVolume"}) (ValueInt 32)]}))
exampleQuery :: IO (Result Value)
exampleQuery = buildResolver @IO @CatOrDog catOrDog (query "{ ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } }")

-- | 'unionValue' can be used in a list context
-- >>> exampleListQuery
-- Result [] (ValueList (List [ValueObject (Object {objectFields = [ObjectField (Name {getNameText = "name"}) (ValueString (String "Felix")),ObjectField (Name {getNameText = "meowVolume"}) (ValueInt 32)]}),ValueObject (Object {objectFields = [ObjectField (Name {getNameText = "name"}) (ValueString (String "Mini")),ObjectField (Name {getNameText = "meowVolume"}) (ValueInt 32)]}),ValueObject (Object {objectFields = [ObjectField (Name {getNameText = "barkVolume"}) (ValueInt 100)]})]))
exampleListQuery :: IO (Result Value)
exampleListQuery = buildResolver @IO @CatOrDogList catOrDogList  (query "{ ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } }")

query :: Text -> Validation.SelectionSet
query q =
  let Right doc = compileQuery q
      Just x = getOperation doc Nothing
  in x
