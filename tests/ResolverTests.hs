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
import qualified GraphQL.Internal.AST as AST
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

tests :: IO TestTree
tests = testSpec "TypeAPI" $ do
  describe "tTest" $ do
    it "works in a simple case" $ do
      Right (Success object) <- runExceptT (interpretAnonymousQuery @T tHandler "{ t(x: 12) }")
      encode object `shouldBe` "{\"t\":12}"
    it "complains about missing field" $ do
      Right (PartialSuccess _ errs) <- runExceptT (interpretAnonymousQuery @T tHandler "{ not_a_field }")
      -- TODO: jml thinks this is a really bad error message. Real problem is
      -- that `not_a_field` was provided.
      errs `shouldBe` singleError (ValueMissing (AST.unsafeMakeName "x"))
    it "complains about missing argument" $ do
      Right (PartialSuccess _ errs) <- runExceptT (interpretAnonymousQuery @T tHandler "{ t }")
      errs `shouldBe` singleError (ValueMissing (AST.unsafeMakeName "x"))
