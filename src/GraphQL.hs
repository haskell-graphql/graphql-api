{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | Interface for GraphQL API.
--
-- __Note__: This module is highly subject to change. We're still figuring
-- where to draw the lines and what to expose.
module GraphQL
  (
    -- * Running queries
    interpretQuery
  , interpretAnonymousQuery
  , Response(..)
    -- * Preparing queries then running them
  , makeSchema
  , compileQuery
  , executeQuery
  , QueryError
  , Schema
  , VariableValues
  , Value
  ) where

import Protolude

import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import GraphQL.API (HasObjectDefinition(..), Object, SchemaError(..))
import GraphQL.Internal.Execution
  ( VariableValues
  , ExecutionError
  , substituteVariables
  )
import qualified GraphQL.Internal.Execution as Execution
import qualified GraphQL.Internal.Syntax.AST as AST
import qualified GraphQL.Internal.Syntax.Parser as Parser
import GraphQL.Internal.Validation
  ( QueryDocument
  , SelectionSetByType
  , ValidationErrors
  , validate
  , getSelectionSet
  , VariableValue
  )
import GraphQL.Internal.Output
  ( GraphQLError(..)
  , Response(..)
  , singleError
  )
import GraphQL.Internal.Schema (Schema)
import qualified GraphQL.Internal.Schema as Schema
import GraphQL.Resolver
  ( HasResolver(..)
  , OperationResolverConstraint
  , Result(..)
  , resolveOperation
  )
import GraphQL.Value (Name, Value)

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
  -- | Error in the schema.
  | SchemaError SchemaError
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
  formatError (SchemaError e) =
    "Schema error: " <> formatError e
  formatError (NonObjectResult v) =
    "Query returned a value that is not an object: " <> show v

-- | Execute a GraphQL query.
executeQuery
  :: forall api m fields typeName interfaces.
  ( Object typeName interfaces fields ~ api
  , OperationResolverConstraint m fields typeName interfaces
  )
  => Handler m api -- ^ Handler for the query. This links the query to the code you've written to handle it.
  -> QueryDocument VariableValue  -- ^ A validated query document. Build one with 'compileQuery'.
  -> Maybe Name -- ^ An optional name. If 'Nothing', then executes the only operation in the query. If @Just "something"@, executes the query named @"something".
  -> VariableValues -- ^ Values for variables defined in the query document. A map of 'Variable' to 'Value'.
  -> m Response -- ^ The outcome of running the query.
executeQuery handler document name variables =
  case getOperation document name variables of
    Left e -> pure (ExecutionFailure (singleError e))
    Right operation ->
      toResult
        <$> resolveOperation @m @fields @typeName @interfaces handler operation
  where
    toResult (Result errors object) =
      case NonEmpty.nonEmpty errors of
        Nothing -> Success object
        Just errs -> PartialSuccess object (map toError errs)

-- | Create a GraphQL schema.
makeSchema :: forall api. HasObjectDefinition api => Either QueryError Schema
makeSchema = first SchemaError (Schema.makeSchema <$> getDefinition @api)

-- | Interpet a GraphQL query.
--
-- Compiles then executes a GraphQL query.
interpretQuery
  :: forall api m fields typeName interfaces.
  ( Object typeName interfaces fields ~ api
  , OperationResolverConstraint m fields typeName interfaces
  )
  => Handler m api -- ^ Handler for the query. This links the query to the code you've written to handle it.
  -> Text -- ^ The text of a query document. Will be parsed and then executed.
  -> Maybe Name -- ^ An optional name for the operation within document to run. If 'Nothing', execute the only operation in the document. If @Just "something"@, execute the query or mutation named @"something"@.
  -> VariableValues -- ^ Values for variables defined in the query document. A map of 'Variable' to 'Value'.
  -> m Response -- ^ The outcome of running the query.
interpretQuery handler query name variables =
  case makeSchema @api >>= flip compileQuery query of
    Left err -> pure (PreExecutionFailure (toError err :| []))
    Right document -> executeQuery @api @m handler document name variables

-- | Interpret an anonymous GraphQL query.
--
-- Anonymous queries have no name and take no variables.
interpretAnonymousQuery
  :: forall api m fields typeName interfaces.
  ( Object typeName interfaces fields ~ api
  , OperationResolverConstraint m fields typeName interfaces
  )
  => Handler m api -- ^ Handler for the anonymous query.
  -> Text -- ^ The text of the anonymous query. Should defined only a single, unnamed query operation.
  -> m Response -- ^ The result of running the query.
interpretAnonymousQuery handler query = interpretQuery @api @m handler query Nothing mempty

-- | Turn some text into a valid query document.
compileQuery :: Schema -> Text -> Either QueryError (QueryDocument VariableValue)
compileQuery schema query = do
  parsed <- first ParseError (parseQuery query)
  first ValidationError (validate schema parsed)

-- | Parse a query document.
parseQuery :: Text -> Either Text AST.QueryDocument
parseQuery query = first toS (parseOnly (Parser.queryDocument <* endOfInput) query)

-- | Get an operation from a query document ready to be processed.
getOperation :: QueryDocument VariableValue -> Maybe Name -> VariableValues -> Either QueryError (SelectionSetByType Value)
getOperation document name vars = first ExecutionError $ do
  op <- Execution.getOperation document name
  resolved <- substituteVariables op vars
  pure (getSelectionSet resolved)
