{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Demonstrate input object usage.
module Main (main) where

import Protolude hiding (Enum)

import qualified Data.Aeson as Aeson

import GraphQL
import GraphQL.API
import GraphQL.Resolver (Handler)
import GraphQL.Value (FromValue, toValue)

data DogStuff = DogStuff { _toy :: Text, _likesTreats :: Bool } deriving (Show, Generic)
instance FromValue DogStuff
instance HasAnnotatedInputType DogStuff
instance Defaultable DogStuff where
  -- TODO defaultFor takes a Name which makes sense, but what's the
  -- name for an input object?
  defaultFor _ = Just (DogStuff "shoe" False)

type Query = Object "Query" '[]
  '[ Argument "dogStuff" DogStuff :> Field "description" Text ]

root :: Handler IO Query
root = pure description

description :: DogStuff -> Handler IO Text
description (DogStuff toy likesTreats)
  | likesTreats = pure $ "likes treats and their favorite toy is a " <> toy
  | otherwise = pure $ "their favorite toy is a " <> toy

-- | Show input object usage
--
-- >>> response <- example "{ description(dogStuff: {toy: \"bone\", likesTreats: true}) }"
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"description":"likes treats and their favorite toy is a bone"}}
--
-- >>> response <- example "{ description }"
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"description":"their favorite toy is a shoe"}}
example :: Text -> IO Response
example = interpretAnonymousQuery @Query root


main :: IO ()
main = do
  response <- example "{ description(dogStuff: {_toy: \"bone\", _likesTreats: true}) }"
  putStrLn $ Aeson.encode $ toValue response
  response' <- example "{ description }"
  putStrLn $ Aeson.encode $ toValue response'
