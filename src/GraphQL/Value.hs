{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Literal GraphQL values.
module GraphQL.Value
  (
    -- | GraphQL values
    GraphQL.Value.Value(..)
  , ToValue(..)
  , Name
  , List
  , Map(..)
  , String
    -- | Fields
  , Field(Field)
  , makeField
  , FieldSet
  , GraphQL.Value.empty
  , singleton
  , fromList
  , mapFromList
  , unionMap
--  , fieldSetToMap
  , union
  , unions
  ) where

import Protolude hiding (Map)

import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.GraphQL.AST (Name)
import Data.Aeson (ToJSON(..), (.=), pairs)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import GHC.Generics (Generic)

-- | Concrete GraphQL value. Essentially Data.GraphQL.AST.Value, but without
-- the "variable" field.
data Value
  = ValueInt Int32
  | ValueFloat Double
  | ValueBoolean Bool
  | ValueString String
  | ValueEnum Name
  | ValueList List
  -- TODO: the output must be ordered in query order, and Map is
  -- ordered in key order. We should probably use a list of tuples for
  -- representation.
  | ValueMap Map
  | ValueNull
  deriving (Eq, Ord, Show)

instance ToJSON GraphQL.Value.Value where

  toJSON (ValueInt x) = toJSON x
  toJSON (ValueFloat x) = toJSON x
  toJSON (ValueBoolean x) = toJSON x
  toJSON (ValueString x) = toJSON x
  toJSON (ValueEnum x) = toJSON x
  toJSON (ValueList x) = toJSON x
  toJSON (ValueMap x) = toJSON x
  toJSON ValueNull = Aeson.Null

newtype String = String Text deriving (Eq, Ord, Show)

instance ToJSON String where
  toJSON (String x) = toJSON x

newtype List = List [GraphQL.Value.Value] deriving (Eq, Ord, Show)

makeList :: (Functor f, Foldable f, ToValue a) => f a -> List
makeList = List . toList . map toValue


instance ToJSON List where
  toJSON (List x) = toJSON x

-- XXX: This is ObjectValue [ObjectField]; ObjectField Name Value upstream.
-- XXX: GraphQL spec itself sometimes says 'map' and other times 'object', but
-- jml hasn't read 100% clearly. Let's find something and stick to it, and
-- make sure that there isn't a real distinction between to the two.
--
-- TODO: the "map" needs to keep track of insertion order so we can
-- return fields in query order as mandated by the spec.
newtype Map = Map [(Name,  GraphQL.Value.Value)] deriving (Eq, Ord, Show, Generic, Monoid)


mapFromList :: [(Name,  GraphQL.Value.Value)] -> Map
mapFromList = Map

-- TODO this would be nicer with a prism `_ValueMap` but don't want to
-- pull in lens as dependency.
unionMap :: [Value] -> Either Text Value
unionMap values = map (ValueMap . fold)  (sequence (map isValueMap values))
  where
    isValueMap = \case
      (ValueMap m) -> Right m
      _ -> Left "non-ValueMap member"


instance ToJSON Map where
  -- Direct encoding to preserve order of keys / values
  toJSON (Map xs) = toJSON (Map.fromList xs)
  toEncoding (Map xs) = pairs (fold (map (\(k, v) -> (toS k) .= v) xs))


data Field = Field Name GraphQL.Value.Value deriving (Eq, Show, Ord)

makeField :: (StringConv name Name, ToValue value) => name -> value -> Field
makeField name value = Field (toS name) (toValue value)

-- TODO I'm unclear about what we're going to use FieldSet for. Do we
-- need it?
data FieldSet = FieldSet (Set Field) deriving (Eq, Show)

--instance ToValue FieldSet where
--  toValue = toValue . fieldSetToMap


-- fieldSetToMap :: FieldSet -> Map
-- fieldSetToMap (FieldSet fields) = Map (Map.fromList [ (name, value) | Field name value <- toList fields ])

empty :: FieldSet
empty = FieldSet Set.empty

singleton :: Field -> FieldSet
singleton = FieldSet . Set.singleton

-- TODO: Fail on duplicate keys.
union :: Alternative m => FieldSet -> FieldSet -> m FieldSet
union (FieldSet x) (FieldSet y) = pure (FieldSet (Set.union x y))

-- TODO: Fail on duplicate keys.
unions :: (Monad m, Alternative m) => [FieldSet] -> m FieldSet
unions = foldrM union GraphQL.Value.empty

-- TODO: Fail on duplicate keys.
fromList :: Alternative m => [Field] -> m FieldSet
fromList = pure . FieldSet . Set.fromList


-- | Turn a Haskell value into a GraphQL value.
class ToValue a where
  toValue :: a -> GraphQL.Value.Value

instance ToValue GraphQL.Value.Value where
  toValue = identity

-- XXX: Should this just be for Foldable?
instance ToValue a => ToValue [a] where
  toValue = toValue . List . map toValue

instance ToValue a => ToValue (NonEmpty a) where
  toValue = toValue . makeList

instance ToValue a => ToValue (Maybe a) where
  toValue = maybe ValueNull toValue

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
  toValue = ValueList

instance ToValue Map where
  toValue = ValueMap

-- XXX: No "enum" instance because not sure what that would be in Haskell.
