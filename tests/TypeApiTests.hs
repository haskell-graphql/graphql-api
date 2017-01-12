{-# LANGUAGE DataKinds, TypeOperators, ScopedTypeVariables #-}
module TypeApiTests (tests) where

import Protolude hiding (Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL
  ( SelectionSet
  , compileQuery
  , getOperation
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
  , buildResolver
  , (:<>)(..)
  , Result(..)
  )
import GraphQL.Value (Value)
import qualified GraphQL.Internal.AST as AST
import Data.Aeson (encode)

-- Test a custom error monad
-- TODO: I didn't realize that MonadThrow throws in the base monad (IO).
type TMonad = ExceptT Text IO
type T = Object "T" '[] '[ Field "z" Int32
                         , Argument "x" Int32 :> Field "t" Int32
                         , Argument "y" Int32 :> Field "q" Int32
                         ]

tHandler :: Handler TMonad T
tHandler =
  pure $ (pure 10) :<> (\tArg -> pure tArg) :<> (pure . (*2))

getQuery :: Text -> SelectionSet Value
getQuery query = either (panic . show) identity $ do
  validated <- compileQuery query
  getOperation validated Nothing mempty

runQuery :: SelectionSet Value -> IO (Either Text (Result Value))
runQuery query = runExceptT (buildResolver @TMonad @T tHandler query)

tests :: IO TestTree
tests = testSpec "TypeAPI" $ do
  describe "tTest" $ do
    it "works in a simple case" $ do
      let query = getQuery "{ t(x: 12) }"
      Right (Result _ r) <- runQuery query
      encode r `shouldBe` "{\"t\":12}"
    it "complains about missing field" $ do
      -- TODO: Apparently MonadThrow throws in the *base monad*,
      -- i.e. usually IO. If we want to throw in the wrapper monad I
      -- think we may need to use MonadFail??
      let wrongQuery = getQuery "{ not_a_field }"
      Right (Result errs _) <- runQuery wrongQuery
      -- TODO: jml thinks this is a really bad error message. Real problem is
      -- that `not_a_field` was provided.
      errs `shouldBe` [ValueMissing (AST.unsafeMakeName "x")]
    it "complains about missing argument" $ do
      let wrongQuery = getQuery "{ t }"
      Right (Result errs _) <- runQuery wrongQuery
      errs `shouldBe` [ValueMissing (AST.unsafeMakeName "x")]
