{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.GraphQL () where

import Protolude

-- | ** LANGUAGE

-- | Names of things in GraphQL.
--
-- /[_A-Za-z][_0-9A-Za-z]*/
newtype Name (name :: Names) = Name Text
data Names = OpName

type OperationName = Name 'OpName


data QueryDocument = QueryDocument [Definition]

data Definition = OpDef OperationDefinition | FragDef FragmentDefinition

data OperationDefinition
  = OperationType (Maybe OperationName) (Maybe VariableDefinitions) (Maybe Directives) SelectionSet
  | SelectionSet

-- | The operations on GraphQL models.
data OperationType
  = Query -- ^ a read-only fetch
  | Mutation -- ^ a write followed by a fetch
  deriving (Eq, Show)

data VariableDefinitions
data Directives

data SelectionSet = SelectionSet [Selection]

data Selection = Field | FragmentSpread | InlineFragment

data FragmentDefinition
