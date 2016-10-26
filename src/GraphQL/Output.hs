-- | GraphQL output.
--
-- How we encode GraphQL responses.
module GraphQL.Output
  ( Response
  ) where

import GraphQL.Value (Value)

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
type Response = Value
