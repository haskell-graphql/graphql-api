{-# LANGUAGE FlexibleContexts #-}
module GraphQL.Internal.Execution (executeRequest) where

import Protolude

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map

import GraphQL.Value (Value(..), astToValue, objectFromOrderedMap)
import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.AST (Name(..))
import GraphQL.Internal.OrderedMap (OrderedMap)
import GraphQL.Internal.Schema (ObjectTypeDefinition)
import GraphQL.Internal.Output (Response(..), Error(..))
import GraphQL.Internal.Validation (getOperationName)

--data Request = Request Schema Document (Maybe Operation) (Maybe VariableValues) InitialValue

-- | Make a non-empty list. This is just an alias for the symbolic constructor.
singleton :: a -> NonEmpty a
singleton x = x :| []

-- | Execute a GraphQL request.
--
-- We don't *actually* want to do this here, probably. Instead, we want to use
-- the type-level goodness of the 'GraphQL.Server' module. However,
-- implementing all the bits of the GraphQL spec is probably the easiest route
-- toward getting there.
--
-- ExecuteRequest(schema, document, operationName, variableValues, initialValue)
--   1. Let operation be the result of GetOperation(document, operationName).
--   2. Let coercedVariableValues be the result of CoerceVariableValues(schema, operation, variableValues).
--   3. If operation is a query operation:
--      a. Return ExecuteQuery(operation, schema, coercedVariableValues, initialValue).
--   4. Otherwise if operation is a mutation operation:
--      a. Return ExecuteMutation(operation, schema, coercedVariableValues, initialValue).
executeRequest :: AST.QueryDocument -> Maybe Name -> VariableValues -> Value -> Response
executeRequest document operationName variableValues initialValue =
  case getOperation document operationName of
    Nothing -> ExecutionFailure (singleton (Error "No such operation $operationName" []))
    Just operation ->
      case coerceVariableValues operation variableValues of
        Left err -> ExecutionFailure (singleton (Error (formatError err) []))
        Right coercedVariableValues ->
          Success . objectFromOrderedMap $ case operation of
            AST.Query (AST.Node _ _ _ ss) -> executeSelectionSet ss initialValue coercedVariableValues
            AST.Mutation (AST.Node _ _ _ ss) -> executeSelectionSet ss initialValue coercedVariableValues
            AST.AnonymousQuery ss -> executeSelectionSet ss initialValue coercedVariableValues

-- | Execute a selection set.
--
-- ExecuteSelectionSet(selectionSet, objectType, objectValue, variableValues)
--   1. Let groupedFieldSet be the result of CollectFields(objectType, selectionSet, variableValues).
--   2. Initialize resultMap to an empty ordered map.
--   3. For each groupedFieldSet as responseKey and fields:
--     a. Let fieldName be the name of the first entry in fields. Note: This value is unaffected if an alias is used.
--     b. Let fieldType be the return type defined for the field fieldName of objectType.
--     c. If fieldType is null:
--       i. Continue to the next iteration of groupedFieldSet.
--     d. Let responseValue be ExecuteField(objectType, objectValue, fields, fieldType, variableValues).
--     e. Set responseValue as the value for responseKey in resultMap.
--   4. Return resultMap.
executeSelectionSet :: AST.SelectionSet -> Value -> VariableValues -> OrderedMap Name Value
executeSelectionSet selectionSet objectValue variableValues =
  let groupedFieldSet = collectFields selectionSet variableValues
  in map (executeFields objectValue variableValues) groupedFieldSet

collectFields :: AST.SelectionSet -> VariableValues -> OrderedMap Name (NonEmpty AST.Field)
collectFields = notImplemented

executeFields :: Value -> VariableValues -> NonEmpty AST.Field  -> Value
executeFields = notImplemented

-- | Get an operation from a GraphQL document
--
-- https://facebook.github.io/graphql/#sec-Executing-Requests
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
getOperation :: AST.QueryDocument -> Maybe Name -> Maybe AST.OperationDefinition
getOperation document = getOperation' (getOperations document)
  where
    -- XXX: By this point we should have validated that
    -- a) list of operations is not empty
    -- b) no name is duplicated
    --
    -- TODO: encode this assumption into types.
    getOperation' :: [AST.OperationDefinition] -> Maybe Name -> Maybe AST.OperationDefinition
    getOperation' operations name@(Just _) = find ((name ==) . getOperationName) operations
    getOperation' [op] Nothing = pure op
    getOperation' _ Nothing = empty

    getOperations :: AST.QueryDocument -> [AST.OperationDefinition]
    getOperations (AST.QueryDocument defns) = [ op | AST.DefinitionOperation op <- defns ]


-- | Top-level definition of a server schema.
type SchemaDefinition = ObjectTypeDefinition

-- | Variable values.
--
-- GraphQL allows the values of variables to be specified, but doesn't provide
-- a way for doing so in the language.
type VariableValues = Map Name Value

data ExecutionError
  = InvalidDefault Name AST.Value
  | MissingValue Name
  deriving (Eq, Show)

formatError :: ExecutionError -> Text
formatError (InvalidDefault name value) = "Could not get default value for " <> show name <> ". Invalid value: " <> show value
formatError (MissingValue name) = "Missing value for " <> show name <> " and must be non-null."

coerceVariableValues :: AST.OperationDefinition -> VariableValues -> Either ExecutionError VariableValues
coerceVariableValues op = coerceVariableValues' (getVariableDefinitions op)

getVariableDefinitions :: AST.OperationDefinition -> [AST.VariableDefinition]
getVariableDefinitions (AST.Query (AST.Node _ defns _ _)) = defns
getVariableDefinitions (AST.Mutation (AST.Node _ defns _ _)) = defns
getVariableDefinitions (AST.AnonymousQuery _) = []

-- | Coerce variable values.
--
-- Given a list of variable definitions @variableDefinitions@ and some
-- provided values @variableValues@, return either a map with all of the
-- variables and their values or an error.
--
-- Note that the GraphQL specification also has this logic do "coercion", i.e.
-- transforming variables from GraphQL values to domain-specific values. We
-- don't do that here, since Haskell doesn't handle heterogeneous collections
-- easily.
--
-- https://facebook.github.io/graphql/#sec-Coercing-Variable-Values
coerceVariableValues' :: [AST.VariableDefinition] -> VariableValues -> Either ExecutionError VariableValues
coerceVariableValues' variableDefinitions variableValues =
  Map.fromList <$> traverse getValue variableDefinitions
  where

    getValue :: AST.VariableDefinition -> Either ExecutionError (Name, Value)
    getValue (AST.VariableDefinition (AST.Variable variableName) variableType defaultValue) =
      (,) <$> pure variableName <*> getValue' variableName variableType defaultValue

    getValue' :: Name -> AST.Type -> Maybe AST.DefaultValue -> Either ExecutionError Value
    getValue' variableName variableType defaultValue =
      case Map.lookup variableName variableValues of
        Just value -> pure value
        Nothing ->
          case defaultValue of
            Just value -> note (InvalidDefault variableName value) (astToValue value)
            Nothing ->
              case variableType of
                (AST.TypeNonNull _) -> throwError (MissingValue variableName)
                _ -> pure ValueNull
