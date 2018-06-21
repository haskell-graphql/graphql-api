{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Implement the \"Execution\" part of the GraphQL spec.
--
-- Actually, most of the execution work takes place in 'GraphQL.Resolver', but
-- there's still a fair bit required to glue together the results of
-- 'GraphQL.Internal.Validation' and the processing in 'GraphQL.Resolver'.
-- This module provides that glue.
module GraphQL.Internal.Execution
  ( VariableValues
  , ExecutionError(..)
  , formatError
  , getOperation
  , substituteVariables
  ) where

import Protolude

import qualified Data.Map as Map
import GraphQL.Value
  ( Name
  , Value
  , pattern ValueNull
  , Value'(..)
  , List'(..)
  , Object'(..)
  )
import GraphQL.Internal.Output (GraphQLError(..))
import GraphQL.Internal.Schema
  ( AnnotatedType (TypeNonNull)
  )
import GraphQL.Internal.Validation
  ( Operation
  , QueryDocument(..)
  , VariableDefinition(..)
  , VariableValue
  , Variable
  )

-- | Get an operation from a GraphQL document
--
-- <https://facebook.github.io/graphql/#sec-Executing-Requests>
--
-- GetOperation(document, operationName):
--
--   * If {operationName} is {null}:
--     * If {document} contains exactly one operation.
--       * Return the Operation contained in the {document}.
--     * Otherwise produce a query error requiring {operationName}.
--   * Otherwise:
--     * Let {operation} be the Operation named {operationName} in {document}.
--     * If {operation} was not found, produce a query error.
--     * Return {operation}.
getOperation :: QueryDocument value -> Maybe Name -> Either ExecutionError (Operation value)
getOperation (LoneAnonymousOperation op) Nothing = pure op
getOperation (MultipleOperations ops) (Just name) = note (NoSuchOperation name) (Map.lookup (pure name) ops)
getOperation (MultipleOperations ops) Nothing =
  case toList ops of
    [op] -> pure op
    _ -> throwError NoAnonymousOperation
getOperation _ (Just name) = throwError (NoSuchOperation name)


-- | Substitute variables in a GraphQL document.
--
-- Once this is done, there will be no variables in the document whatsoever.
substituteVariables :: Operation VariableValue -> VariableValues -> Either ExecutionError (Operation Value)
substituteVariables op vars = traverse (replaceVariable vars) op

replaceVariable :: VariableValues -> VariableValue -> Either ExecutionError Value
replaceVariable vars value =
  case value of
    ValueScalar' (Left defn) -> getValue defn
    ValueScalar' (Right v) -> pure (ValueScalar' v)
    ValueList' (List' xs) -> ValueList' . List' <$> traverse (replaceVariable vars) xs
    ValueObject' (Object' xs) -> ValueObject' . Object' <$> traverse (replaceVariable vars) xs
  where

    getValue :: VariableDefinition -> Either ExecutionError Value
    getValue (VariableDefinition variableName variableType defaultValue) =
      note (MissingValue variableName) $
      Map.lookup variableName vars <|> defaultValue <|> allowNull variableType

    allowNull (TypeNonNull _) = empty
    allowNull _ = pure ValueNull

-- | An error that occurs while executing a query. Technically,
-- 'ResolverError' also falls into the same category, but is separate to help
-- our code be a bit better organized.
data ExecutionError
  = MissingValue Variable
  | NoSuchOperation Name
  | NoAnonymousOperation
  deriving (Eq, Show)

instance GraphQLError ExecutionError where
  formatError (MissingValue name) = "Missing value for " <> show name <> " and must be non-null."
  formatError (NoSuchOperation name) = "Requested operation " <> show name <> " but couldn't find it."
  formatError NoAnonymousOperation = "No name supplied for opertaion, but no anonymous operation."

-- | A map of variables to their values.
--
-- In GraphQL the variable values are not part of the query itself, they are
-- instead passed in through a separate channel. Create a 'VariableValues'
-- from this other channel and pass it to 'substituteVariables'.
--
-- GraphQL allows the values of variables to be specified, but doesn't provide
-- a way for doing so in the language.
type VariableValues = Map Variable Value
