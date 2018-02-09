-- | Description: Implement handlers for GraphQL schemas
--
-- Contains everything you need to write handlers for your GraphQL schema.
module GraphQL.Resolver
  ( ResolverError(..)
  , HasResolver(..)
  , (:<>)(..)
  , Result(..)
  , unionValue
  ) where

import GraphQL.Internal.Resolver
  ( ResolverError(..)
  , HasResolver(..)
  , (:<>)(..)
  , Result(..)
  , unionValue
  )
