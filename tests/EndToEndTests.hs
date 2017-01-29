{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Tests that span the entire system.
--
-- These tests function both as examples of how to use the API, as well as
-- sanity checks on our reasoning.
module EndToEndTests (tests) where

import Protolude

import Data.Aeson (Value(Null), toJSON, object, (.=))
import qualified Data.Map as Map
import GraphQL (makeSchema, compileQuery, executeQuery, interpretAnonymousQuery, interpretQuery)
import GraphQL.API (Object, Field)
import GraphQL.Internal.Syntax.AST (Variable(..))
import GraphQL.Resolver ((:<>)(..), Handler)
import GraphQL.Value (makeName)
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
doesKnowCommand dog command = command `elem` knownCommands dog

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
    it "Handles nested queries" $ do
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
                , "owner" .= object
                  [ "name" .= ("jml" :: Text)
                  ]
                ]
              ]
            ]
      toJSON (toValue response) `shouldBe` expected
    it "It aliases fields" $ do
      let root = pure (viewServerDog mortgage)
      let query = [r|{
                      dog {
                        name
                        boss: owner {
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
                , "boss" .= object
                  [ "name" .= ("jml" :: Text)
                  ]
                ]
              ]
            ]
      toJSON (toValue response) `shouldBe` expected
    it "Passes arguments to functions" $ do
      let root = pure (viewServerDog mortgage)
      let query = [r|{
                      dog {
                        name
                        doesKnowCommand(dogCommand: Sit)
                      }
                     }
                    |]
      response <- interpretAnonymousQuery @QueryRoot root query
      let expected =
            object
            [ "data" .= object
              [ "dog" .= object
                [ "name" .= ("Mortgage" :: Text)
                , "doesKnowCommand" .= False
                ]
              ]
            ]
      toJSON (toValue response) `shouldBe` expected
    it "Handles fairly complex queries" $ do
      let root = pure (viewServerDog mortgage)
      -- TODO: jml would like to put some union checks in here, but we don't
      -- have any unions reachable from Dog!
      let query = [r|{
                      dog {
                        callsign: name
                        ... on Dog {
                          callsign: name
                          me: owner {
                            ... on Sentient {
                              name
                            }
                            ... on Human {
                              name
                            }
                            name
                          }
                        }
                      }
                     }
                    |]
      response <- interpretAnonymousQuery @QueryRoot root query
      let expected =
            object
            [ "data" .= object
              [ "dog" .= object
                [ "callsign" .= ("Mortgage" :: Text)
                , "me" .= object
                  [ "name" .= ("jml" :: Text)
                  ]
                ]
              ]
            ]
      toJSON (toValue response) `shouldBe` expected
  describe "interpretQuery" $ do
    it "Handles the simplest named query" $ do
      let root = pure (viewServerDog mortgage)
      let query = [r|query myQuery {
                      dog {
                        name
                      }
                    }
                   |]
      response <- interpretQuery @QueryRoot root query Nothing mempty
      let expected =
            object
            [ "data" .= object
              [ "dog" .= object
                [ "name" .= ("Mortgage" :: Text)
                ]
              ]
            ]
      toJSON (toValue response) `shouldBe` expected
    it "Allows calling query by name" $ do
      let root = pure (viewServerDog mortgage)
      let query = [r|query myQuery {
                      dog {
                        name
                      }
                    }
                   |]
      let Right name = makeName "myQuery"
      response <- interpretQuery @QueryRoot root query (Just name) mempty
      let expected =
            object
            [ "data" .= object
              [ "dog" .= object
                [ "name" .= ("Mortgage" :: Text)
                ]
              ]
            ]
      toJSON (toValue response) `shouldBe` expected
    describe "Handles variables" $ do
      let root = pure (viewServerDog mortgage)
      let Right schema = makeSchema @Dog
      let Right query =
            compileQuery schema
            [r|query myQuery($whichCommand: DogCommand) {
                 dog {
                   name
                   doesKnowCommand(dogCommand: $whichCommand)
                 }
               }
              |]
      it "Errors when no variables provided" $ do
        response <- executeQuery  @QueryRoot root query Nothing mempty
        let expected =
              object
              [ "data" .= object
                [ "dog" .= object
                  [ "name" .= ("Mortgage" :: Text)
                  , "doesKnowCommand" .= Null
                  ]
                ]
              , "errors" .=
                [
                  object
                  -- TODO: This error message is pretty bad. We should define
                  -- a typeclass for client-friendly "Show" (separate from
                  -- actual Show which remains extremely useful for debugging)
                  -- and use that when including values in error messages.
                  [ "message" .= ("Could not coerce Name {unName = \"dogCommand\"} to valid value: ValueScalar' ConstNull not an enum: [Right (Name {unName = \"Sit\"}),Right (Name {unName = \"Down\"}),Right (Name {unName = \"Heel\"})]" :: Text)
                  ]
                ]
              ]
        toJSON (toValue response) `shouldBe` expected
      it "Substitutes variables when they are provided" $ do
        -- TODO: This is a crummy way to make a variable map. jml doesn't want
        -- to come up with a new API in this PR, but probably we should have a
        -- very simple function to turn a JSON value / object into the
        -- variable map that we desire. Alternatively, we should have APIs
        -- like Aeson does.
        -- <https://github.com/jml/graphql-api/issues/96>
        let Right varName = makeName "whichCommand"
        let vars = Map.singleton (Variable varName) (toValue Sit)
        response <- executeQuery  @QueryRoot root query Nothing vars
        let expected =
              object
              [ "data" .= object
                [ "dog" .= object
                  [ "name" .= ("Mortgage" :: Text)
                  , "doesKnowCommand" .= False
                  ]
                ]
              ]
        toJSON (toValue response) `shouldBe` expected

