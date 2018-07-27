{-# LANGUAGE AllowAmbiguousTypes #-}
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
  , interpretRequest
  , interpretAnonymousQuery
  , Response(..)
    -- * Preparing queries then running them
  , makeSchema
  , compileQuery
  , executeQuery
  , executeRequest
  , QueryError
  , Schema
  , SchemaRoot(..)
  , VariableValues
  , Value
  ) where

import Protolude

import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import GraphQL.API (HasObjectDefinition(..), SchemaError(..))
import GraphQL.Internal.Execution
  ( VariableValues
  , ExecutionError(..)
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
  , Operation(..)
  , DefinitionType(..)
  , getDefinitionType
  )
import GraphQL.Internal.Output
  ( GraphQLError(..)
  , Response(..)
  , singleError
  )
import GraphQL.Internal.Schema (Schema)
import qualified GraphQL.Internal.Schema as Schema
import GraphQL.Resolver (HasResolver(..), Result(..))
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

toResult :: Result Value -> Response
toResult (Result errors result) = case result of
  -- TODO: Prevent this at compile time. Particularly frustrating since
  -- we *know* that queries and mutations have object definitions
  ValueObject object ->
    case NonEmpty.nonEmpty errors of
      Nothing -> Success object
      Just errs -> PartialSuccess object (map toError errs)
  v -> ExecutionFailure (singleError (NonObjectResult v))

-- | Execute a GraphQL query.
executeQuery
  :: forall api m. (HasResolver m api, Applicative m, HasObjectDefinition api)
  => Handler m api -- ^ Handler for the query. This links the query to the code you've written to handle it.
  -> QueryDocument VariableValue  -- ^ A validated query document. Build one with 'compileQuery'.
  -> Maybe Name -- ^ An optional name. If 'Nothing', then executes the only operation in the query. If @Just "something"@, executes the query named @"something".
  -> VariableValues -- ^ Values for variables defined in the query document. A map of 'Variable' to 'Value'.
  -> m Response -- ^ The outcome of running the query.
executeQuery handler document name variables =
  case getOperation document name variables of
    Left e -> pure (ExecutionFailure (singleError e))
    Right (_, ss) -> toResult <$> resolve @m @api handler (Just ss)

-- | Create a GraphQL schema.
makeSchema :: forall api. HasObjectDefinition api => Either QueryError Schema
makeSchema = first SchemaError (Schema.makeSchema <$> getDefinition @api)

-- | Interpet a GraphQL query.
--
-- Compiles then executes a GraphQL query.
interpretQuery
  :: forall api m. (Applicative m, HasResolver m api, HasObjectDefinition api)
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
  :: forall api m. (Applicative m, HasResolver m api, HasObjectDefinition api)
  => Handler m api -- ^ Handler for the anonymous query.
  -> Text -- ^ The text of the anonymous query. Should defined only a single, unnamed query operation.
  -> m Response -- ^ The result of running the query.
interpretAnonymousQuery handler query = interpretQuery @api @m handler query Nothing mempty

data SchemaRoot m query mutation = SchemaRoot
  { queries   :: Handler m query
  , mutations :: Handler m mutation
  }

-- | Execute a query or mutation
--
-- Similar to executeQuery, execept requests are dispatched against the
-- SchemaRoot depending on whether they are a query or mutation
executeRequest
  :: forall schema queries mutations m.
  ( schema ~ SchemaRoot m queries mutations
  , HasResolver m queries
  , HasObjectDefinition queries
  , HasResolver m mutations
  , HasObjectDefinition mutations
  , Monad m
  )
  => SchemaRoot m queries mutations
  -> QueryDocument VariableValue
  -> Maybe Name
  -> VariableValues
  -> m Response
executeRequest (SchemaRoot qh mh) document name variables =
  case getOperation document name variables of
    Left e -> pure (ExecutionFailure (singleError e))
    Right (operation, ss) -> do
      toResult <$> case operation of
        Query    _ _ _ -> resolve @m @queries qh (Just ss)
        Mutation _ _ _ -> resolve @m @mutations mh (Just ss)

-- | Interpret a query or mutation against a SchemaRoot
interpretRequest
  :: forall schema queries mutations m.
  ( schema ~ SchemaRoot m queries mutations
  , HasResolver m queries
  , HasObjectDefinition queries
  , HasResolver m mutations
  , HasObjectDefinition mutations
  , Monad m
  )
  => SchemaRoot m queries mutations
  -> Text
  -> Maybe Name
  -> VariableValues
  -> m Response
interpretRequest (SchemaRoot qh mh) text name variables = case parseQuery text of
  Left err -> pure (PreExecutionFailure (toError (ParseError err) :| []))
  Right document ->
    case getDefinitionType document name of
      Just operation -> case operation of
        QueryDefinition    -> run @m @queries qh document
        MutationDefinition -> run @m @mutations mh document
      _ -> 
        let err = maybe NoAnonymousOperation NoSuchOperation name
        in pure (ExecutionFailure (toError err :| []))
    where
      run :: forall n api.
          ( HasObjectDefinition api
          , HasResolver n api
          , Applicative n
          )
          => Handler n api -> AST.QueryDocument -> n Response
      run h doc = case makeSchema @api of
        Left e -> pure (PreExecutionFailure (toError e :| []))
        Right schema -> case validate schema doc of
          Left e -> pure (PreExecutionFailure (toError (ValidationError e) :| []))
          Right validated -> executeQuery @api h validated name variables

-- | Turn some text into a valid query document.
compileQuery :: Schema -> Text -> Either QueryError (QueryDocument VariableValue)
compileQuery schema query = do
  parsed <- first ParseError (parseQuery query)
  first ValidationError (validate schema parsed)

-- | Parse a query document.
parseQuery :: Text -> Either Text AST.QueryDocument
parseQuery query = first toS (parseOnly (Parser.queryDocument <* endOfInput) query)

-- | Get an operation from a query document ready to be processed.
getOperation :: QueryDocument VariableValue -> Maybe Name -> VariableValues -> Either QueryError (Operation VariableValue, SelectionSetByType Value)
getOperation document name vars = first ExecutionError $ do
  op <- Execution.getOperation document name
  resolved <- substituteVariables op vars
  pure (op, getSelectionSet resolved)