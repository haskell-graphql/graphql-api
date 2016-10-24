-- | GraphQL inputs & outputs.
--
-- We need ways to encode GraphQL responses, and also to 'evaluate' incoming
-- requests into canonical form.
--
-- Note: 'Application' is a terrible name.
module GraphQL.Application
  ( CanonicalQuery
  , Response
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.GraphQL.AST as AST

-- | A query that has all its fragments, variables, and directives evaluated,
-- so that all that is left is a query with literal values.
--
-- 'SelectionSet' is maybe the closest type, but isn't quite what we want, as
-- it still has places for directives and other symbolic values.
type CanonicalQuery = AST.SelectionSet

-- | GraphQL response.
--
-- A GraphQL response must:
--
--   * be a map
--   * have a "data" key iff the operation executed
--   * have an "errors" key iff the operation encountered errors
--   * not include "data" if operation failed before execution (e.g. syntax errors,
--     validation errors, missing info)
--   * not have keys other than "data", "errors", and "extensions"
--
-- Other interesting things:
--
--   * Doesn't have to be JSON, but does have to have maps, strings, lists,
--     and null
--   * Can also support bool, int, enum, and float
--   * Value of "extensions" must be a map
--
-- "data" must be null if an error was encountered during execution that
-- prevented a valid response.
--
-- "errors"
--
--   * must be a non-empty list
--   * each error is a map with "message", optionally "locations" key
--     with list of locations
--   * locations are maps with 1-indexed "line" and "column" keys.
type Response = Aeson.Value
