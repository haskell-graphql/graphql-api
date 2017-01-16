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
catOrDogList = pure
  [ unionValue @MiniCat (miniCat "Felix")
  , unionValue @MiniCat (miniCat "Mini")
  , unionValue @MiniDog miniDog
  ]

-- $setup
-- >>> import qualified GraphQL.Internal.Encoder as Encode
-- >>> import GraphQL.Value (valueToAST)


-- | Show usage of a single unionValue
--
-- >>> (Result _ result) <- exampleQuery
-- >>> putStrLn $ Encode.value (valueToAST result)
-- {name:"MonadicFelix",meowVolume:32}
exampleQuery :: IO (Result Value)
exampleQuery = buildResolver @IO @CatOrDog catOrDog (query "{ ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } }")

-- | 'unionValue' can be used in a list context
--
-- >>> (Result _ result) <- exampleListQuery
-- >>> putStrLn $ Encode.value (valueToAST result)
-- [{name:"Felix",meowVolume:32},{name:"Mini",meowVolume:32},{barkVolume:100}]
exampleListQuery :: IO (Result Value)
exampleListQuery = buildResolver @IO @CatOrDogList catOrDogList  (query "{ ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } }")

query :: Text -> Validation.SelectionSet Value
query q =
  let Right doc = compileQuery q
      Right x = getOperation doc Nothing mempty
  in x
