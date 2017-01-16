{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.InputObject where
import Protolude hiding (Enum)

import GraphQL
import GraphQL.API
import GraphQL.Resolver (Handler, Defaultable(..))
import GraphQL.Value.FromValue (FromValue)

data DogStuff = DogStuff { toy :: Text, likesTreats :: Bool } deriving (Show, Generic)
instance FromValue DogStuff
instance HasAnnotatedInputType DogStuff
instance Defaultable DogStuff where
  -- TODO defaultFor takes a Name which makes sense, but what's the
  -- name for an input object?
  defaultFor _ = Just (DogStuff "shoe" False)

type Query = Object "Query" '[]
  '[ Argument "dogStuff" DogStuff :> Field "root" Text ]

root :: Handler IO Query
root = pure (\dogStuff -> pure (show dogStuff))

-- $setup
-- >>> import Data.Aeson (encode)
-- >>> import GraphQL.Value.ToValue (ToValue(..))

-- TODO: jml thinks it's a bit confusing to have `show` output in these
-- examples. Mixing between JSON syntax and Haskell's record syntax confuses
-- the point.

-- | Show input object usage
--
-- >>> response <- example
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"root":"DogStuff {toy = \"bone\", likesTreats = True}"}}
example :: IO Response
example = interpretAnonymousQuery @Query root "{ root(dogStuff: {toy: \"bone\", likesTreats: true}) }"

-- | Show that example replacement works
--
-- >>> response <- exampleDefault
-- >>> putStrLn $ encode $ toValue response
-- {"data":{"root":"DogStuff {toy = \"shoe\", likesTreats = False}"}}
exampleDefault :: IO Response
exampleDefault = interpretAnonymousQuery @Query root "{ root }"
