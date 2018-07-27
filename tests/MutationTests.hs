{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module MutationTests (tests) where

import Protolude hiding (Enum)

import Data.IORef
import Text.RawString.QQ (r)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, runIO, before_)
import Utils

import GraphQL
import GraphQL.API
import GraphQL.Resolver

import GraphQL.Internal.Name (Name)
import GraphQL.Internal.Validation (QueryDocument, VariableValue)

data Server = Server
  { counter :: IORef Int32
  }

type Counter = Object "Counter" '[]
  '[ Field "value" Int32
   ]

type QueryRoot = Object "QueryRoot" '[]
  '[ Field "counter" Counter
   ]

queryRoot :: Server -> Handler IO QueryRoot
queryRoot server = pure 
  $ pure (readIORef $ counter server)

type MutationRoot = Object "MutationRoot" '[]
  '[ Field "increment" Counter
   , Argument "to" Int32 :> Field "reset" Counter
   ]

mutationRoot :: Server -> Handler IO MutationRoot
mutationRoot server = pure
    $ pure (atomicModifyIORef' (counter server) $ \n -> (n+1, n+1))
  :<> \n -> pure $ do
      writeIORef (counter server) n
      return n

type CounterSchema = SchemaRoot IO QueryRoot MutationRoot

schemaHandler :: Server -> SchemaRoot IO QueryRoot MutationRoot
schemaHandler server = SchemaRoot (queryRoot server) (mutationRoot server)

tests :: IO TestTree
tests = 
  testSpec "Schema with mutations" $ do
    let Right querySchema = makeSchema @QueryRoot
        Right mutationSchema = makeSchema @MutationRoot

        Right getCount = compileQuery querySchema 
          [r|query getCount { 
            counter {
              value
            }
          }|]
        Right increment = compileQuery mutationSchema 
          [r|mutation increment { 
            increment {
              value
            }
          }|]
  
    server <- runIO $ Server <$> newIORef 0

    let 
      reset :: IO ()
      reset = writeIORef (counter server) 0

      run :: QueryDocument VariableValue -> Maybe Name -> VariableValues -> IO Response
      run = executeRequest @CounterSchema (schemaHandler server)

    describe "execution" $ before_ reset $ do
      it "can issue queries" $ do
        response <- run getCount "getCount" mempty
        response `shouldBeJSON` [json|
          {
            "data": {
              "counter": { "value": 0 }
            }
          }
        |]
  
      it "can issue mutations" $ do
        response <- run increment "increment" mempty
        response `shouldBeJSON` [json|
          {
            "data": {
              "increment": { "value": 1 }
            }
          }
        |]

