{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Turn domain-specific Haskell values into GraphQL values.
module GraphQL.Value.ToValue
  ( ToValue(..)
  ) where

import Protolude
import GraphQL.Value
import Data.List.NonEmpty (NonEmpty)

-- * ToValue

-- | Turn a Haskell value into a GraphQL value.
class ToValue a where
  toValue :: a -> Value' ConstScalar

instance ToValue (Value' ConstScalar) where
  toValue = identity

-- XXX: Should this just be for Foldable?
instance ToValue a => ToValue [a] where
  toValue = toValue . List' . map toValue

-- TODO - tom still thinks that using Maybe for nullable is maybe not
-- the best idea. <https://github.com/jml/graphql-api/issues/100>
instance ToValue a => ToValue (Maybe a) where
  toValue Nothing = ValueNull
  toValue (Just v) = toValue v

instance ToValue a => ToValue (NonEmpty a) where
  toValue = toValue . makeList

instance ToValue Bool where
  toValue = ValueBoolean

instance ToValue Int32 where
  toValue = ValueInt

instance ToValue Double where
  toValue = ValueFloat

instance ToValue String where
  toValue = ValueString

-- XXX: Make more generic: any string-like thing rather than just Text.
instance ToValue Text where
  toValue = toValue . String

instance ToValue List where
  toValue = ValueList'

instance ToValue (Object' ConstScalar) where
  toValue = ValueObject'


makeList :: (Functor f, Foldable f, ToValue a) => f a -> List
makeList = List' . Protolude.toList . map toValue
