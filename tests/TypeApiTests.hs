{-# LANGUAGE TypeApplications, DataKinds, TypeOperators, ScopedTypeVariables #-}
module TypeApiTests (tests) where

import Protolude hiding (Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.Definitions
import GraphQL.TypeApi
import qualified Data.GraphQL.AST as AST
import Data.Aeson (encode)

import Data.GraphQL.Parser (document)
import Data.Attoparsec.Text (parseOnly, endOfInput)


-- Test a custom error monad
-- TODO: I didn't realize that MonadThrow throws in the base monad (IO).
type TMonad = ExceptT Text IO
type T = Object "T" '[] '[Field "z" Int32, Argument "t" Int32 :> Field "t" Int32]

tHandler :: HandlerType TMonad T
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


-- Run like this: buildResolver @IO @Listed listedHandler listedQuery
type Listed = Object "Listed" '[] '[Field "items" (List Int32)]

listedHandler :: HandlerType IO Listed
listedHandler = pure ([pure 10, pure 20] :<> ())

listedQuery :: AST.SelectionSet
listedQuery =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
        parseOnly (document <* endOfInput) "{ items }"
  in selectionSet


type Calculator = Object "Calculator" '[]
  '[ Argument "a" Int32 :> Argument "b" Int32 :> Field "add" Int32
   , Argument "a" Double :> Field "log" Double
   ]

type API = Object "API" '[] '[Field "calc" Calculator]

type FakeUser = ()

calculatorHandler :: FakeUser -> HandlerType IO Calculator
calculatorHandler _fakeUser =
  pure (add' :<> log' :<> ())
  where
    add' a b = pure (a + b)
    log' a = pure (log a)

api :: HandlerType IO API
api = do
  fakeUser <- print @IO @Text "fake lookup user"
  pure (calculatorHandler fakeUser :<> ())

-- Use like: `buildResolver @IO @Calculator (calculatorHandler ()) calculatorQuery`
calculatorQuery :: AST.SelectionSet
calculatorQuery =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
        parseOnly (document <* endOfInput) "{ add(a: 1, b: 2) }"
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
