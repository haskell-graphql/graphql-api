-- | GraphQL output.
--
-- How we encode GraphQL responses.
module GraphQL.Output
  ( Response(..)
  , Errors
  , Error(..)  -- XXX: Maybe export helper functions rather than constructors.
  ) where

import Protolude hiding (Location, Map)
import Data.List.NonEmpty (NonEmpty)
import GraphQL.Value (Map, mapFromList, ToValue(..), Value(ValueNull))

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
data Response
  = Success Map
  | PreExecutionFailure Errors
  | ExecutionFailure Errors
  | PartialSuccess Map Errors
  deriving (Eq, Ord, Show)

instance ToValue Response where
  toValue (Success x) = toValue (mapFromList [("data", toValue x)])
  toValue (PreExecutionFailure e) = toValue (mapFromList [("errors", toValue e)])
  toValue (ExecutionFailure e) = toValue (mapFromList [("data", ValueNull)
                                                      ,("errors", toValue e)
                                                      ])
  toValue (PartialSuccess x e) = toValue (mapFromList [("data", toValue x)
                                                      ,("errors", toValue e)
                                                      ])

type Errors = NonEmpty Error

data Error = Error Text [Location] deriving (Eq, Ord, Show)

instance ToValue Error where
  toValue (Error message []) = toValue (mapFromList [("message", toValue message)])
  toValue (Error message locations) = toValue (mapFromList [("message", toValue message)
                                                           ,("locations", toValue locations)
                                                           ])

data Location = Location Line Column deriving (Eq, Ord, Show)
type Line = Int32  -- XXX: 1-indexed natural number
type Column = Int32  -- XXX: 1-indexed natural number

instance ToValue Location where
  toValue (Location line column) = toValue (mapFromList [("line" , toValue line)
                                                        ,("column", toValue column)])
