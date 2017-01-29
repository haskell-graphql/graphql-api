{-# LANGUAGE DataKinds #-}
module Examples.UnionExample  where

import Protolude
import GraphQL.API (Field, List, Object, Union)
import GraphQL (Response, interpretAnonymousQuery)
import GraphQL.Resolver (Handler, (:<>)(..), unionValue)

-- Slightly reduced example from the spec
type MiniCat = Object "MiniCat" '[] '[Field "name" Text, Field "meowVolume" Int32]
type MiniDog = Object "MiniDog" '[] '[Field "barkVolume" Int32]

type CatOrDog = Object "Me" '[] '[Field "myPet" (Union "CatOrDog" '[MiniCat, MiniDog])]
type CatOrDogList = Object "CatOrDogList" '[] '[Field "pets" (List (Union "CatOrDog" '[MiniCat, MiniDog]))]

miniCat :: Text -> Handler IO MiniCat
miniCat name = pure (pure name :<> pure 32)

miniDog :: Handler IO MiniDog
miniDog = pure (pure 100)

catOrDog :: Handler IO CatOrDog
catOrDog = pure $ do
  name <- pure "MonadicFelix" -- we can do monadic actions
  unionValue @MiniCat (miniCat name)

catOrDogList :: Handler IO CatOrDogList
catOrDogList = pure $
  pure [ unionValue @MiniCat (miniCat "Felix")
       , unionValue @MiniCat (miniCat "Mini")
       , unionValue @MiniDog miniDog
       ]

-- $setup
-- >>> import Data.Aeson (encode)
-- >>> import GraphQL.Value.ToValue (ToValue(..))

-- | Show usage of a single unionValue
--
-- >>> response <- exampleQuery
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"myPet":{"meowVolume":32,"name":"MonadicFelix"}}}
exampleQuery :: IO Response
exampleQuery = interpretAnonymousQuery @CatOrDog catOrDog "{ myPet { ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } } }"

-- | 'unionValue' can be used in a list context
--
-- >>> response <- exampleListQuery
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"pets":[{"meowVolume":32,"name":"Felix"},{"meowVolume":32,"name":"Mini"},{"barkVolume":100}]}}
exampleListQuery :: IO Response
exampleListQuery = interpretAnonymousQuery @CatOrDogList catOrDogList  "{ pets { ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } } }"
