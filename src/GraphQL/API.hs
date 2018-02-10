-- | Description: Define a GraphQL schema with Haskell types
--
-- Use this to define your GraphQL schema with Haskell types.
module GraphQL.API
  ( Object
  , Field
  , Argument
  , Union
  , List
  , Enum
  , GraphQLEnum(..)
  , Interface
  , (:>)(..)
  , Defaultable(..)
  , HasObjectDefinition(..)
  , HasAnnotatedInputType(..)
  , SchemaError(..)
  ) where

import GraphQL.Internal.API
  ( Object
  , Field
  , Argument
  , Union
  , List
  , Enum
  , GraphQLEnum(..)
  , Interface
  , (:>)(..)
  , Defaultable(..)
  , HasObjectDefinition(..)
  , HasAnnotatedInputType(..)
  , SchemaError(..)
  )
