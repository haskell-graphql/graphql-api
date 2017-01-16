{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Interface for GraphQL API.
--
-- __Note__: This module is highly subject to change. We're still figuring
-- where to draw the lines and what to expose.
module GraphQL
  ( QueryError
  , Response(..)
  , SelectionSet
  , VariableValues
  , Value
  , compileQuery
  , executeQuery
  , interpretQuery
  , interpretAnonymousQuery
  , getOperation
  ) where

import Protolude

import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.Execution
  ( VariableValues
  , ExecutionError
  , substituteVariables
  )
import qualified GraphQL.Internal.Execution as Execution
import qualified GraphQL.Internal.Parser as Parser
import GraphQL.Internal.Validation
  ( QueryDocument
  , SelectionSet
  , ValidationErrors
  , validate
  , getSelectionSet
  , VariableValue
  )
import GraphQL.Internal.Output
  ( GraphQLError(..)
  , Error(..)
  , Response(..)
  , singleError
  )
import GraphQL.Resolver (HasGraph(..), Result(..))
import GraphQL.Value (Name, Value, pattern ValueObject)

-- | Errors that can happen while processing a query document.
data QueryError
  -- | Failed to parse.
  = ParseError Text
  -- | Parsed, but failed validation.
  --
  -- See <https://facebook.github.io/graphql/#sec-Validation> for more
  -- details.
  | ValidationError ValidationErrors
  -- | Validated, but failed during execution.
  | ExecutionError ExecutionError
  -- | Got a value that wasn't an object.
  | NonObjectResult Value
  deriving (Eq, Show)

instance GraphQLError QueryError where
  formatError (ParseError e) =
    "Couldn't parse query document: " <> e
  formatError (ValidationError es) =
    "Validation errors:\n" <> mconcat ["  " <> formatError e <> "\n" | e <- NonEmpty.toList es]
  formatError (ExecutionError e) =
    "Execution error: " <> show e
  formatError (NonObjectResult v) =
    "Query returned a value that is not an object: " <> show v

-- | Execute a GraphQL query.
executeQuery :: forall api m. (HasGraph m api, Applicative m) => Handler m api -> QueryDocument VariableValue -> Maybe Name -> VariableValues -> m Response
executeQuery handler document name variables =
  case getOperation document name variables of
    Left e -> pure (ExecutionFailure (singleError e))
    Right operation ->
      toResult <$> buildResolver @m @api handler operation
  where
    toResult (Result errors result) =
      case result of
        ValueObject object ->
          case NonEmpty.nonEmpty errors of
            Nothing -> Success object
            Just errs -> PartialSuccess object (map toError errs)
        v -> ExecutionFailure (singleError (NonObjectResult v))

-- | Interpet a GraphQL query.
--
-- Compiles then executes a GraphQL query.
interpretQuery :: forall api m. (Applicative m, HasGraph m api) => Handler m api -> Text -> Maybe Name -> VariableValues -> m Response
interpretQuery handler query name variables =
  case parseQuery query of
    Left err -> pure (PreExecutionFailure (Error err [] :| []))
    Right parsed ->
      case validate parsed of
        Left errs -> pure (PreExecutionFailure (map toError errs))
        Right document ->
          executeQuery @api @m handler document name variables


-- | Interpret an anonymous GraphQL query.
--
-- Anonymous queries have no name and take no variables.
interpretAnonymousQuery :: forall api m. (Applicative m, HasGraph m api) => Handler m api -> Text -> m Response
interpretAnonymousQuery handler query = interpretQuery @api @m handler query Nothing mempty

-- | Turn some text into a valid query document.
compileQuery :: Text -> Either QueryError (QueryDocument VariableValue)
compileQuery query = do
  parsed <- first ParseError (parseQuery query)
  first ValidationError (validate parsed)

-- | Parse a query document.
parseQuery :: Text -> Either Text AST.QueryDocument
parseQuery query = first toS (parseOnly (Parser.queryDocument <* endOfInput) query)

-- | Get an operation from a query document ready to be processed.
--
-- TODO: Open question whether we want to export this to the end-user. If we
-- do, it should probably not be in first position.
getOperation :: QueryDocument VariableValue -> Maybe Name -> VariableValues -> Either QueryError (SelectionSet Value)
getOperation document name vars = first ExecutionError $ do
  op <- Execution.getOperation document name
  resolved <- substituteVariables op vars
  pure (getSelectionSet resolved)
