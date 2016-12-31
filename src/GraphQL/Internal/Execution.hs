{-# LANGUAGE FlexibleContexts #-}
module GraphQL.Internal.Execution () where

import Protolude

import qualified Data.Map as Map

import GraphQL.Value (Value(..), astToValue)
import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.AST (Name(..))
import GraphQL.Internal.Schema (ObjectTypeDefinition)
import GraphQL.Internal.Validation (getOperationName)

--data Request = Request Schema Document (Maybe Operation) (Maybe VariableValues) InitialValue

{-
ExecuteRequest(schema, document, operationName, variableValues, initialValue)
  1. Let operation be the result of GetOperation(document, operationName).
  2. Let coercedVariableValues be the result of CoerceVariableValues(schema, operation, variableValues).
  3. If operation is a query operation:
     a. Return ExecuteQuery(operation, schema, coercedVariableValues, initialValue).
  4. Otherwise if operation is a mutation operation:
     a. Return ExecuteMutation(operation, schema, coercedVariableValues, initialValue).

-}



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

data Error
  = InvalidDefault Name AST.Value
  | MissingValue Name
  deriving (Eq, Show)

formatError :: Error -> Text
formatError (InvalidDefault name value) = "Could not get default value for " <> show name <> ". Invalid value: " <> show value
formatError (MissingValue name) = "Missing value for " <> show name <> " and must be non-null."

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
coerceVariableValues :: [AST.VariableDefinition] -> VariableValues -> Either Error VariableValues
coerceVariableValues variableDefinitions variableValues =
  Map.fromList <$> traverse getValue variableDefinitions
  where

    getValue :: AST.VariableDefinition -> Either Error (Name, Value)
    getValue (AST.VariableDefinition (AST.Variable variableName) variableType defaultValue) =
      (,) <$> pure variableName <*> getValue' variableName variableType defaultValue

    getValue' :: Name -> AST.Type -> Maybe AST.DefaultValue -> Either Error Value
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
