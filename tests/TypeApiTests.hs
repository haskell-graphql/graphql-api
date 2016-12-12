{-# LANGUAGE TypeApplications, DataKinds, TypeOperators, ScopedTypeVariables #-}
module TypeApiTests where

import Protolude hiding (Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.Definitions
import GraphQL.TypeApi
import GraphQL.Schema hiding (ValueInt)
import qualified Control.Monad.Trans.Except as E
import qualified Data.GraphQL.AST as AST
import Data.Aeson (encode)

import Data.GraphQL.Parser (document)
import Data.Attoparsec.Text (parseOnly, endOfInput)


-- Test a custom error monad
type TMonad = E.ExceptT Text IO
type T = Object "T" '[] '[Field "z" Int32, Argument "t" Int32 :> Field "t" Int32]


tHandler :: HandlerType TMonad T
tHandler = do
  pure $ (pure 10) :<> (\tArg -> pure tArg) :<> ()


tQuery =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
        parseOnly (document <* endOfInput) "{ t(t: 12) }"
  in selectionSet

tWrongQuery =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
        parseOnly (document <* endOfInput) "{ not_a_field }"
  in selectionSet


-- hlist :: a -> (a :<> ()) TODO
-- hlist a = a :<> ()

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
calculatorQuery =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
        parseOnly (document <* endOfInput) "{ add(a: 1, b: 2) }"
  in selectionSet


typeApiTests :: IO TestTree
typeApiTests = testSpec "Type" $ do
  describe "tTest" $ do
    it "works in a simple case" $ do
      Right r <- E.runExceptT $ buildResolver @TMonad @T tHandler tQuery
      encode r `shouldBe` "{\"t\":12}"
    it "complains on error" $ do
      -- TODO: Apparently MonadThrow throws in the *base monad*,
      -- i.e. usually IO. If we want to throw in the wrapper monad I
      -- think we may need to use MonadFail??
      (void $ E.runExceptT (buildResolver @TMonad @T tHandler tWrongQuery)) `catch` \(e :: QueryError) -> print e
