-- | Representation of GraphQL names.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GraphQL.Internal.Name
  ( Name(unName)
  , NameError(..)
  , makeName
  , nameFromSymbol
  -- * Named things
  , HasName(..)
  -- * Unsafe functions
  , unsafeMakeName
  ) where

import Protolude

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import GraphQL.Internal.Syntax.AST
  ( Name(..)
  , NameError(..)
  , unsafeMakeName
  , makeName
  )

-- | Convert a type-level 'Symbol' into a GraphQL 'Name'.
nameFromSymbol :: forall (n :: Symbol). KnownSymbol n => Either NameError Name
nameFromSymbol = makeName (toS (symbolVal @n Proxy))

-- | Types that implement this have values with a single canonical name in a
-- GraphQL schema.
--
-- e.g. a field @foo(bar: Int32)@ would have the name @\"foo\"@.
--
-- If a thing *might* have a name, or has a name that might not be valid,
-- don't use this.
--
-- If a thing is aliased, then return the *original* name.
class HasName a where
  -- | Get the name of the object.
  getName :: a -> Name
