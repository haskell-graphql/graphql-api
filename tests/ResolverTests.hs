{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module ResolverTests (tests) where

import Protolude hiding (Enum)

import Data.Aeson.QQ (aesonQQ)
import Text.RawString.QQ (r)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import Data.Aeson (encode, toJSON)
import GraphQL
  ( Response(..)
  , interpretAnonymousQuery
  )
import GraphQL.API
  ( Object
  , Field
  , Argument
  , Enum
  , Union
  , (:>)
  )
import GraphQL.Resolver
  ( Handler
  , ResolverError(..)
  , (:<>)(..)
  , unionValue
  )
import GraphQL.Internal.Output (singleError)

import EnumTests ( Mode(NormalFile) )

-- Test a custom error monad
type TMonad = ExceptT Text IO
type T = Object "T" '[] '[ Field "z" Int32
                         , Argument "x" Int32 :> Field "t" Int32
                         , Argument "y" Int32 :> Field "q" Int32
                         ]

tHandler :: Handler TMonad T
tHandler =
  pure $ (pure 10) :<> (\tArg -> pure tArg) :<> (pure . (*2))


-- https://github.com/jml/graphql-api/issues/119
-- Maybe X didn't descend into its argument. Now it does.
type Query = Object "Query" '[]
  '[ Argument "id" Text :> Field "test" (Maybe Foo) ]

type Foo = Object "Foo" '[]
  '[ Field "name" Text ]

data ServerFoo = ServerFoo
  { name :: Text
  } deriving (Eq, Show)

lookupFoo :: Text -> IO (Maybe ServerFoo)
lookupFoo _ = pure $ Just (ServerFoo "Mort")

viewFoo :: ServerFoo -> Handler IO Foo
viewFoo ServerFoo { name=name } = pure $ pure $ name

handler :: Handler IO Query
handler = pure $ \fooId -> do
  foo <- lookupFoo fooId
  -- note that fmap maps over the Maybe, so we still need
  -- have to wrap the result in a pure.
  sequence $ fmap (pure . viewFoo) foo

-- Enum test
type EnumQuery = Object "File" '[]
  '[ Field "mode" (Enum "modeEnumName" Mode) ]

enumHandler :: Handler IO EnumQuery
enumHandler = pure $ pure NormalFile
-- /Enum test

-- Union test
type Cat = Object "Cat" '[] '[Field "name" Text]
type Dog = Object "Dog" '[] '[Field "name" Text]
type CatOrDog = Union "CatOrDog" '[Cat, Dog]
type UnionQuery = Object "UnionQuery" '[]
  '[ Argument "isCat" Bool :> Field "catOrDog" CatOrDog
   ]

dogHandler :: Handler IO Cat
dogHandler = pure $ pure "Mortgage"

catHandler :: Handler IO Dog
catHandler = pure $ pure "Felix"

unionHandler :: Handler IO UnionQuery
unionHandler = pure $ \isCat ->
  if isCat
    then unionValue @Cat catHandler
    else unionValue @Dog dogHandler

-- /Union test

tests :: IO TestTree
tests = testSpec "TypeAPI" $ do
  describe "tTest" $ do
    it "works in a simple case" $ do
      Right (Success object) <- runExceptT (interpretAnonymousQuery @T tHandler "{ t(x: 12) }")
      encode object `shouldBe` "{\"t\":12}"
    it "complains about missing field" $ do
      Right (PartialSuccess _ errs) <- runExceptT (interpretAnonymousQuery @T tHandler "{ not_a_field }")
      errs `shouldBe` singleError (FieldNotFoundError "not_a_field")
    it "complains about missing argument" $ do
      Right (PartialSuccess _ errs) <- runExceptT (interpretAnonymousQuery @T tHandler "{ t }")
      errs `shouldBe` singleError (ValueMissing "x")
  describe "issue 119" $ do
    it "Just works" $ do
      Success object <- interpretAnonymousQuery @Query handler "{ test(id: \"10\") { name } }"
      encode object `shouldBe` "{\"test\":{\"name\":\"Mort\"}}"
  describe "Parse, validate and execute queries against API" $ do
    it "API.Enum works" $ do
      Success object <- interpretAnonymousQuery @EnumQuery enumHandler "{ mode }"
      encode object `shouldBe` "{\"mode\":\"NormalFile\"}"

  describe "Introspection" $ do
    describe "__typename" $ do
      it "can describe nested objects" $ do
        Success object <- interpretAnonymousQuery @Query handler [r|
          { 
            __typename
            test(id: "1") { 
              __typename 
              name
            } 
          }
        |]

        toJSON object `shouldBe` [aesonQQ|
          { 
            "__typename": "Query",
            "test": {
              "__typename": "Foo",
              "name": "Mort"
            }
          }
        |]

      it "can describe unions" $ do
        Success object <- interpretAnonymousQuery @UnionQuery unionHandler [r|
          {
            __typename
            catOrDog(isCat: false) {
              __typename
              name
            }
          }
        |]

        toJSON object `shouldBe` [aesonQQ|
          {
            "__typename": "UnionQuery",
            "catOrDog": {
              "__typename": "Dog",
              "name": "Mortgage"
            }
          }
        |]
