{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Tests that span the entire system.
--
-- These tests function both as examples of how to use the API, as well as
-- sanity checks on our reasoning.
module EndToEndTests (tests) where

import Protolude

import Data.Aeson (Value(Null), toJSON, object, (.=))
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

-- | Our server's internal representation of a 'Dog'.
data ServerDog
  = ServerDog
    { name :: Text
    , nickname :: Maybe Text
    , barkVolume :: Int32
    , knownCommands :: Set DogCommand
    , houseTrainedAtHome :: Bool
    , houseTrainedElsewhere :: Bool
    , owner :: ServerHuman
    }

-- | Whether 'ServerDog' knows the given command.
doesKnowCommand :: ServerDog -> DogCommand -> Bool
doesKnowCommand dog command = command `elem` (knownCommands dog)

-- | Whether 'ServerDog' is house-trained.
isHouseTrained :: ServerDog -> Maybe Bool -> Bool
isHouseTrained dog Nothing = houseTrainedAtHome dog || houseTrainedElsewhere dog
isHouseTrained dog (Just False) = houseTrainedAtHome dog
isHouseTrained dog (Just True) = houseTrainedElsewhere dog

-- | Present 'ServerDog' for GraphQL.
viewServerDog :: ServerDog -> Handler IO Dog
viewServerDog dog@(ServerDog{..}) = pure $
  pure name :<>
  pure nickname :<>
  pure barkVolume :<>
  pure . doesKnowCommand dog :<>
  pure . isHouseTrained dog :<>
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

-- | Our server's internal representation of a 'Human'.
data ServerHuman = ServerHuman Text deriving (Eq, Ord, Show)

-- | Present a 'ServerHuman' as a GraphQL 'Human'.
viewServerHuman :: ServerHuman -> Handler IO Human
viewServerHuman (ServerHuman name) = pure (pure name)

-- | It me.
jml :: ServerHuman
jml = ServerHuman "jml"

tests :: IO TestTree
tests = testSpec "End-to-end tests" $ do
  describe "interpretAnonymousQuery" $ do
    it "Handles the simplest possible valid query" $ do
      let root = pure (viewServerDog mortgage)
      let query = [r|{
                      dog {
                        name
                      }
                    }
                   |]
      response <- interpretAnonymousQuery @QueryRoot root query
      let expected =
            object
            [ "data" .= object
              [ "dog" .= object
                [ "name" .= ("Mortgage" :: Text)
                ]
              ]
            ]
      toJSON (toValue response) `shouldBe` expected
    it "Handles more than one field" $ do
      let root = pure (viewServerDog mortgage)
      let query = [r|{
                      dog {
                        name
                        barkVolume
                      }
                    }
                   |]
      response <- interpretAnonymousQuery @QueryRoot root query
      let expected =
            object
            [ "data" .= object
              [ "dog" .= object
                [ "name" .= ("Mortgage" :: Text)
                , "barkVolume" .= (0 :: Int32)
                ]
              ]
            ]
      toJSON (toValue response) `shouldBe` expected
    it "TODO: Handles nested queries" $ do
      let root = pure (viewServerDog mortgage)
      let query = [r|{
                      dog {
                        name
                        owner {
                          name
                        }
                      }
                    }
                   |]
      response <- interpretAnonymousQuery @QueryRoot root query
      let expected =
            object
            [ "data" .= object
              [ "dog" .= object
                [ "name" .= ("Mortgage" :: Text)
                , "owner" .= Null
                ]
              ]
            , "errors" .=
              [ object
                [ "message" .= ("No value provided for Name {getNameText = \"dogCommand\"}, and no default specified." :: Text)
                ]
              ]
            ]
      toJSON (toValue response) `shouldBe` expected
