{-# LANGUAGE PatternSynonyms #-}
-- | GraphQL output.
--
-- How we encode GraphQL responses.
module GraphQL.Internal.Output
  ( Response(..)
  , Errors
  , Error(..)  -- XXX: Maybe export helper functions rather than constructors.
  , GraphQLError(..)
  ) where

import Protolude hiding (Location, Map)
import Data.List.NonEmpty (NonEmpty)
import GraphQL.Value
  ( Object
  , objectFromList
  , Value
  , pattern ValueObject
  , pattern ValueNull
  )
import GraphQL.Internal.AST (NameError(..), unsafeMakeName)
import GraphQL.Value.ToValue (ToValue(..))

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
  = Success Object
  | PreExecutionFailure Errors
  | ExecutionFailure Errors
  | PartialSuccess Object Errors
  deriving (Eq, Ord, Show)

-- | Construct an object from a list of names and values.
--
-- Panic if there are duplicate names.
unsafeMakeObject :: HasCallStack => [(Text, Value)] -> Value
unsafeMakeObject fields =
  case objectFromList (map (first unsafeMakeName) fields) of
    Nothing -> panic $ "Object has duplicate keys: " <> show fields
    Just object -> ValueObject object

instance ToValue Response where
  toValue (Success x) = unsafeMakeObject [("data", toValue x)]
  toValue (PreExecutionFailure e) = unsafeMakeObject [("errors", toValue e)]
  toValue (ExecutionFailure e) = unsafeMakeObject [("data", ValueNull)
                                                  ,("errors", toValue e)]
  toValue (PartialSuccess x e) = unsafeMakeObject [("data", toValue x)
                                                  ,("errors", toValue e)
                                                  ]

type Errors = NonEmpty Error

data Error = Error Text [Location] deriving (Eq, Ord, Show)

instance ToValue Error where
  toValue (Error message []) = unsafeMakeObject [("message", toValue message)]
  toValue (Error message locations) = unsafeMakeObject [("message", toValue message)
                                                       ,("locations", toValue locations)
                                                       ]

data Location = Location Line Column deriving (Eq, Ord, Show)
type Line = Int32  -- XXX: 1-indexed natural number
type Column = Int32  -- XXX: 1-indexed natural number

instance ToValue Location where
  toValue (Location line column) = unsafeMakeObject [("line" , toValue line)
                                                    ,("column", toValue column)
                                                    ]

-- | An error that arises while processing a GraphQL query.
class GraphQLError e where
  -- | Represent an error as human-readable text, primarily intended for
  -- developers of GraphQL clients, and secondarily for developers of GraphQL
  -- servers.
  formatError :: e -> Text

  -- | Represent an error as human-readable text, together with reference to a
  -- series of locations within a GraphQL query document. Default
  -- implementation calls 'formatError' and provides no locations.
  toError :: e -> Error
  toError e = Error (formatError e) []

-- Defined here to avoid circular dependency.
instance GraphQLError NameError where
  formatError (NameError name) = "Not a valid GraphQL name: " <> show name
