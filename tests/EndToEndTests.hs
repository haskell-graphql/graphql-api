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

data ServerDog
  = ServerDog
    { name :: Text
      -- XXX: Causes later compilation error.
      --    /Users/jml/src/graphql-api/tests/EndToEndTests.hs:94:19: error:
      --        • No instance for (GraphQL.Resolver.HasGraph IO (Maybe Text))
      --            arising from a use of ‘interpretAnonymousQuery’
    , nickname :: Maybe Text
    , barkVolume :: Int
      -- XXX: This is wrong. I don't want my internal implementation to be in
      -- terms of 'Enum "DogCommand" DogCommandEnum'--that's a graphql thing.
      -- I want it to be DogCommandEnum. i.e. one of 'Sit', 'Down', or 'Heel'.
    , knownCommands :: Set DogCommand
    , houseTrainedAtHome :: Bool
    , houseTrainedElsewhere :: Bool
    , owner :: ServerHuman
    }

doesKnowCommand :: ServerDog -> DogCommand -> Bool
doesKnowCommand dog command = command `elem` (knownCommands dog)

isHouseTrained :: ServerDog -> Maybe Bool -> Bool
isHouseTrained dog Nothing = houseTrainedAtHome dog || houseTrainedElsewhere dog
isHouseTrained dog (Just False) = houseTrainedAtHome dog
isHouseTrained dog (Just True) = houseTrainedElsewhere dog

viewServerDog :: ServerDog -> Handler IO Dog
viewServerDog dog@(ServerDog{..}) = pure $
  pure @IO name :<>
  pure @IO nickname :<>
  pure @IO barkVolume :<>
  -- XXX: Currently fails with compilation error:
  --
  -- /Users/jml/src/graphql-api/tests/EndToEndTests.hs:59:3: error:
  --     • The type family ‘Handler’ should have 2 arguments, but has been given 1
  --     • In the first argument of ‘(.)’, namely ‘pure @(Handler IO)’
  --       In the first argument of ‘(:<>)’, namely
  --         ‘pure @(Handler IO) . doesKnowCommand dog’
  --       In the second argument of ‘(:<>)’, namely
  --         ‘pure @(Handler IO) . doesKnowCommand dog
  --          :<>
  --            pure @(Handler IO) . isHouseTrained dog :<> viewServerHuman owner’
  pure @(Handler IO) . doesKnowCommand dog :<>
  pure @(Handler IO) . isHouseTrained dog :<>
  viewServerHuman owner

-- | jml has a stuffed black dog called "Mortgage".
mortgage :: ServerDog
mortgage = ServerDog
           { name = "Mortgage"
           , nickname = Just "Mort"
           , barkVolume = 0  -- He's stuffed
           , knownCommands = mempty  -- He's stuffed
           , houseTrainedAtHome = True  -- Never been a problem
           , houseTrainedElsewhere = True  -- Untested in the field
           , owner = jml
           }

data ServerHuman = ServerHuman Text deriving (Eq, Ord, Show)

viewServerHuman :: ServerHuman -> Handler IO Human
viewServerHuman (ServerHuman name) = pure (pure name)

jml :: ServerHuman
jml = ServerHuman "jml"

tests :: IO TestTree
tests = testSpec "End-to-end tests" $ do
  describe "Simple query" $ do
    it "Returns what we expect" $ do
      let root = pure (viewServerDog mortgage)
      let query = [r|{
                      dog {
                        name
                      }
                    }
                   |]
      response <- interpretAnonymousQuery @QueryRoot root query
      toJSON (toValue response) `shouldBe` Null
