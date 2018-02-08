{-# LANGUAGE DataKinds #-}
module Main (main) where

import Protolude

import qualified Data.Aeson as Aeson
import GraphQL.API (Field, List, Object, Union)
import GraphQL (interpretAnonymousQuery)
import GraphQL.Resolver (Handler, (:<>)(..), unionValue)
import GraphQL.Value (ToValue(..))

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

main :: IO ()
main = do
  response <- interpretAnonymousQuery @CatOrDog catOrDog "{ myPet { ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } } }"
  putStrLn $ Aeson.encode $ toValue response
  response' <- interpretAnonymousQuery @CatOrDogList catOrDogList "{ pets { ... on MiniCat { name meowVolume } ... on MiniDog { barkVolume } } }"
  putStrLn $ Aeson.encode $ toValue response'
