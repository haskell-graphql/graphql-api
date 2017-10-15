{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module ResolverTests (tests) where

import Protolude hiding (Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import Data.Aeson (encode)
import GraphQL
  ( Response(..)
  , interpretAnonymousQuery
  )
import GraphQL.API
  ( Object
  , Field
  , Argument
  , (:>)
  )
import GraphQL.Resolver
  ( Handler
  , ResolverError(..)
  , (:<>)(..)
  )
import GraphQL.Internal.Output (singleError)

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
