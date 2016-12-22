{-# LANGUAGE DataKinds, TypeOperators, ScopedTypeVariables #-}
module TypeApiTests (tests) where

import Protolude hiding (Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.API
  ( Object
  , Field
  , Argument
  , (:>)
  )
import GraphQL.Server
  ( Handler
  , QueryError(..)
  , buildResolver
  , (:<>)(..)
  )
import qualified GraphQL.Internal.AST as AST
import GraphQL.Value (Value)
import Data.Aeson (encode)

import GraphQL.Internal.Parser (document)
import Data.Attoparsec.Text (parseOnly, endOfInput)

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

getQuery :: Text -> AST.SelectionSet
getQuery query =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
        parseOnly (document <* endOfInput) query
  in selectionSet

runQuery :: AST.SelectionSet -> IO (Either Text Value)
runQuery query = runExceptT (buildResolver @TMonad @T tHandler query)

tests :: IO TestTree
tests = testSpec "TypeAPI" $ do
  describe "tTest" $ do
    it "works in a simple case" $ do
      let query = getQuery "{ t(x: 12) }"
      Right r <- runQuery query
      encode r `shouldBe` "{\"t\":12}"
    it "complains about missing field" $ do
      -- TODO: Apparently MonadThrow throws in the *base monad*,
      -- i.e. usually IO. If we want to throw in the wrapper monad I
      -- think we may need to use MonadFail??
      let wrongQuery = getQuery "{ not_a_field }"
      caught <- (runQuery wrongQuery >> pure Nothing) `catch` \(e :: QueryError) -> pure (Just e)
      caught `shouldBe` Just (QueryError "Query for undefined selection: SelectionField (Field \"\" \"not_a_field\" [] [] [])")
