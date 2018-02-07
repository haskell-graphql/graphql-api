{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.InputObject where
import Protolude hiding (Enum)

import GraphQL
import GraphQL.API
import GraphQL.Resolver (Handler)
import GraphQL.Value.FromValue (FromValue)

data DogStuff = DogStuff { toy :: Text, likesTreats :: Bool } deriving (Show, Generic)
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

-- $setup
-- >>> import Data.Aeson (encode)
-- >>> import GraphQL.Value.ToValue (ToValue(..))

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
