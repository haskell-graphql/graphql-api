-- | Interface for GraphQL API.
--
-- __Note__: This module is highly subject to change. We're still figuring
-- where to draw the lines and what to expose.
module GraphQL
  ( QueryError
  , SelectionSet
  , getOperation
  , processQuery
  ) where

import Protolude

import Data.Attoparsec.Text (parseOnly, endOfInput)
import qualified GraphQL.Internal.AST as AST
import qualified GraphQL.Internal.Parser as Parser
import qualified GraphQL.Internal.Validation
import GraphQL.Internal.Validation
  ( QueryDocument
  , SelectionSet
  , ValidationErrors
  , validate
  , getSelectionSet
  )

-- | Errors that can happen while processing a query document.
data QueryError
  -- | Failed to parse.
  = ParseError Text
  -- | Parsed, but failed validation.
  --
  -- See <https://facebook.github.io/graphql/#sec-Validation> for more
  -- details.
  | ValidationError ValidationErrors
  deriving (Eq, Show)

-- | Turn some text into a valid query document.
--
-- TODO: jml hates the name of this function, \"process\" is so meaningless.
processQuery :: Text -> Either QueryError QueryDocument
processQuery query = do
  parsed <- first ParseError (parseQuery query)
  first ValidationError (validate parsed)

-- | Parse a query document.
parseQuery :: Text -> Either Text AST.QueryDocument
parseQuery query = first toS (parseOnly (Parser.queryDocument <* endOfInput) query)

-- | Get an operation from a query document ready to be processed.
--
-- TODO: This is the wrong API. For example, it doesn't take variable values.
getOperation :: QueryDocument -> Maybe AST.Name -> Maybe SelectionSet
getOperation document name = do
  op <- GraphQL.Internal.Validation.getOperation document name
  pure (getSelectionSet op)
