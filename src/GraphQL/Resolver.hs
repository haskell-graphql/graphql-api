-- | Description: Implement handlers for GraphQL schemas
--
-- Contains everything you need to write handlers for your GraphQL schema.
module GraphQL.Resolver
  ( module Export
  ) where

import GraphQL.Internal.Resolver as Export
  ( ResolverError(..)
  , HasResolver(..)
  , OperationResolverConstraint
  , (:<>)(..)
  , Result(..)
  , unionValue
  , resolveOperation
  )
