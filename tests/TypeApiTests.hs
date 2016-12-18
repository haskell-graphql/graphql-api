{-# LANGUAGE TypeApplications, DataKinds, TypeOperators, ScopedTypeVariables #-}
module TypeApiTests (tests) where

import Protolude hiding (Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.TypedSchema
  ( Object
  , Field
  , Argument
  , (:>)
  )
import GraphQL.TypedApi
  ( Handler
  , QueryError(..)
  , buildResolver
  , (:<>)(..)
  )
import qualified Data.GraphQL.AST as AST
import Data.Aeson (encode)

import Data.GraphQL.Parser (document)
import Data.Attoparsec.Text (parseOnly, endOfInput)


-- Test a custom error monad
-- TODO: I didn't realize that MonadThrow throws in the base monad (IO).
type TMonad = ExceptT Text IO
type T = Object "T" '[] '[Field "z" Int32, Argument "t" Int32 :> Field "t" Int32]

tHandler :: Handler TMonad T
tHandler =
  pure $ (pure 10) :<> (\tArg -> pure tArg) :<> ()


tQuery :: AST.SelectionSet
tQuery =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
        parseOnly (document <* endOfInput) "{ t(t: 12) }"
  in selectionSet

tWrongQuery :: AST.SelectionSet
tWrongQuery =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
        parseOnly (document <* endOfInput) "{ not_a_field }"
  in selectionSet


tests :: IO TestTree
tests = testSpec "Type" $ do
  describe "tTest" $ do
    it "works in a simple case" $ do
      Right r <- runExceptT $ buildResolver @TMonad @T tHandler tQuery
      encode r `shouldBe` "{\"t\":12}"
    it "complains on error" $ do
      -- TODO: Apparently MonadThrow throws in the *base monad*,
      -- i.e. usually IO. If we want to throw in the wrapper monad I
      -- think we may need to use MonadFail??
      caught <- (runExceptT (buildResolver @TMonad @T tHandler tWrongQuery) >> pure Nothing) `catch` \(e :: QueryError) -> pure (Just e)
      caught `shouldBe` Just (QueryError "Query for undefined selection:SelectionField (Field \"\" \"not_a_field\" [] [] [])")
