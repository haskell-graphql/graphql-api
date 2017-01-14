-- | Interface for GraphQL API.
--
-- __Note__: This module is highly subject to change. We're still figuring
-- where to draw the lines and what to expose.
module GraphQL
  ( QueryError
  , SelectionSet
  , VariableValues
  , Value
  , getOperation
  , compileQuery
  ) where

import Protolude

import Data.Attoparsec.Text (parseOnly, endOfInput)
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
import GraphQL.Value (Value)

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
  deriving (Eq, Show)

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
getOperation :: QueryDocument VariableValue -> Maybe AST.Name -> VariableValues -> Either QueryError (SelectionSet Value)
getOperation document name vars = first ExecutionError $ do
  op <- Execution.getOperation document name
  resolved <- substituteVariables op vars
  pure (getSelectionSet resolved)
