-- | Description: Literal GraphQL values
{-# LANGUAGE PatternSynonyms #-}
module GraphQL.Value
  ( Value
  , Value'(..)
  , ConstScalar
  , UnresolvedVariableValue
  , pattern ValueInt
  , pattern ValueFloat
  , pattern ValueBoolean
  , pattern ValueString
  , pattern ValueEnum
  , pattern ValueList
  , pattern ValueObject
  , pattern ValueNull
  , toObject
  , valueToAST
  , astToVariableValue
  , variableValueToAST
  , List
  , List'(..)
  , String(..)
    -- * Names
  , Name(..)
  , NameError(..)
  , makeName
    -- * Objects
  , Object
  , Object'(..)
  , ObjectField
  , ObjectField'(ObjectField)
    -- ** Constructing
  , makeObject
  , objectFromList
  , objectFromOrderedMap
    -- ** Combining
  , unionObjects
    -- ** Querying
  , objectFields
  ) where

import GraphQL.Internal.Value
  ( Value
  , Value'(..)
  , ConstScalar
  , UnresolvedVariableValue
  , pattern ValueInt
  , pattern ValueFloat
  , pattern ValueBoolean
  , pattern ValueString
  , pattern ValueEnum
  , pattern ValueList
  , pattern ValueObject
  , pattern ValueNull
  , toObject
  , valueToAST
  , astToVariableValue
  , variableValueToAST
  , List
  , List'(..)
  , String(..)
  , Name(..)
  , NameError(..)
  , makeName
  , Object
  , Object'(..)
  , ObjectField
  , ObjectField'(ObjectField)
  , makeObject
  , objectFromList
  , objectFromOrderedMap
  , unionObjects
  , objectFields
  )
