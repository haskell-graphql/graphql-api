{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.InputObject where
import Protolude hiding (Enum)

import GraphQL
import GraphQL.API
import GraphQL.Resolver (Handler, Result(..), (:<>)(..), buildResolver, Defaultable(..))
import GraphQL.Value (Value)
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
-- >>> import qualified GraphQL.Internal.Encoder as Encode
-- >>> import GraphQL.Value (valueToAST)

-- | Show input object usage
--
-- >>> (Result _ result) <- example
-- >>> putStrLn $ Encode.value (valueToAST result)
-- {root:"DogStuff {toy = \"bone\", likesTreats = True}"}
example :: IO (Result Value)
example = buildResolver @IO @Query root (query "{ root(dogStuff: {toy: \"bone\", likesTreats: true}) }")

-- | Show that example replacement works
--
-- >>> (Result _ result) <- exampleDefault
-- >>> putStrLn $ Encode.value (valueToAST result)
-- {root:"DogStuff {toy = \"shoe\", likesTreats = False}"}
exampleDefault :: IO (Result Value)
exampleDefault = buildResolver @IO @Query root (query "{ root }")

query :: Text -> SelectionSet Value
query q = either (panic . show) identity $ do
  document <- compileQuery q
  getOperation document Nothing mempty
