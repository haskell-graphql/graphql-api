{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Tests that span the entire system.
--
-- These tests function both as examples of how to use the API, as well as
-- sanity checks on our reasoning.
module EndToEndTests (tests) where

import Protolude

import Data.Aeson (Value(Null), toJSON)
import GraphQL (interpretAnonymousQuery)
import GraphQL.API (Object, Field)
import GraphQL.Resolver ((:<>)(..), Handler)
import GraphQL.Value.ToValue (ToValue(..))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)
import Text.RawString.QQ (r)

import ExampleSchema

-- | Example query root.
--
-- @
-- type QueryRoot {
--   dog: Dog
-- }
-- @
--
-- Drawn from <https://facebook.github.io/graphql/#sec-Validation>.
type QueryRoot = Object "QueryRoot" '[]
  '[ Field "dog" Dog
   ]

data DogConfig
  = DogConfig
    { name :: Text
      -- XXX: Causes later compilation error.
      --    /Users/jml/src/graphql-api/tests/EndToEndTests.hs:94:19: error:
      --        • No instance for (GraphQL.Resolver.HasGraph IO (Maybe Text))
      --            arising from a use of ‘interpretAnonymousQuery’
    , nickname :: Maybe Text
    , barkVolume :: Int32
      -- XXX: This is wrong. I don't want my internal implementation to be in
      -- terms of 'Enum "DogCommand" DogCommandEnum'--that's a graphql thing.
      -- I want it to be DogCommandEnum. i.e. one of 'Sit', 'Down', or 'Heel'.
    , knownCommands :: Set DogCommandEnum
    , houseTrainedAtHome :: Bool
    , houseTrainedElsewhere :: Bool
    , owner :: HumanConfig
    }

doesKnowCommand :: DogConfig -> DogCommandEnum -> IO Bool
doesKnowCommand dog command = pure $ command `elem` (knownCommands dog)

isHouseTrained :: DogConfig -> Maybe Bool -> IO Bool
isHouseTrained dog Nothing = pure $ houseTrainedAtHome dog || houseTrainedElsewhere dog
isHouseTrained dog (Just False) = pure $ houseTrainedAtHome dog
isHouseTrained dog (Just True) = pure $ houseTrainedElsewhere dog

viewDogConfig :: DogConfig -> Handler IO Dog
viewDogConfig dog@(DogConfig{..}) = pure $
  pure name :<>
  pure nickname :<>
  pure barkVolume :<>
--  doesKnowCommand dog :<>
--  isHouseTrained dog :<>
  viewHumanConfig owner

-- | jml has a stuffed black dog called "Mortgage".
mortgage :: DogConfig
mortgage = DogConfig
           { name = "Mortgage"
           , nickname = Just "Mort"
           , barkVolume = 0  -- He's stuffed
           , knownCommands = mempty  -- He's stuffed
           , houseTrainedAtHome = True  -- Never been a problem
           , houseTrainedElsewhere = True  -- Untested in the field
           , owner = jml
           }

data HumanConfig = HumanConfig Text deriving (Eq, Ord, Show)

viewHumanConfig :: HumanConfig -> Handler IO Human
viewHumanConfig (HumanConfig name) = pure (pure name)

jml :: HumanConfig
jml = HumanConfig "jml"

tests :: IO TestTree
tests = testSpec "End-to-end tests" $ do
  describe "Simple query" $ do
    it "Returns what we expect" $ do
      let root = pure (viewDogConfig mortgage)
      let query = [r|{
                      dog {
                        name
                      }
                    }
                   |]
      response <- interpretAnonymousQuery @QueryRoot root query
      toJSON (toValue response) `shouldBe` Null
