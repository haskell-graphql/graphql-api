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

import qualified Data.Attoparsec.Text as A
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import GraphQL.Internal.Syntax.AST
  ( Name(..)
  , nameParser
  )

-- | An invalid name.
newtype NameError = NameError Text deriving (Eq, Show)

-- | Create a 'Name'.
--
-- Names must match the regex @[_A-Za-z][_0-9A-Za-z]*@. If the given text does
-- not match, return Nothing.
--
-- >>> makeName "foo"
-- Right (Name {unName = "foo"})
-- >>> makeName "9-bar"
-- Left (NameError "9-bar")
makeName :: Text -> Either NameError Name
makeName name = first (const (NameError name)) (A.parseOnly nameParser name)

-- | Convert a type-level 'Symbol' into a GraphQL 'Name'.
nameFromSymbol :: forall (n :: Symbol). KnownSymbol n => Either NameError Name
nameFromSymbol = makeName (toS (symbolVal @n Proxy))

-- | Create a 'Name', panicking if the given text is invalid.
--
-- Prefer 'makeName' to this in all cases.
--
-- >>> unsafeMakeName "foo"
-- Name {unName = "foo"}
unsafeMakeName :: HasCallStack => Text -> Name
unsafeMakeName name =
  case makeName name of
    Left e -> panic (show e)
    Right n -> n

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
