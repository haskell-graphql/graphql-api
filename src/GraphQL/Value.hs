{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Literal GraphQL values.
module GraphQL.Value
  (
    -- | GraphQL values
    Value(..)
  , ToValue(..)
  , Name
  , List
  , Map
  , String
    -- | Fields
  , Field(Field)
  , makeField
  , FieldSet
  , GraphQL.Value.empty
  , singleton
  , fromList
  , union
  , unions
  ) where

import Protolude hiding (Map)

import Data.Foldable (foldrM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.GraphQL.AST (Name)
import Data.Aeson (ToJSON(..))

-- | Concrete GraphQL value. Essentially Data.GraphQL.AST.Value, but without
-- the "variable" field.
data Value
  = ValueInt Int32
  | ValueFloat Double
  | ValueBoolean Bool
  | ValueString String
  | ValueEnum Name
  | ValueList List
  | ValueMap Map
  deriving (Eq, Ord, Show)

instance ToJSON Value where

  toJSON (ValueInt x) = toJSON x
  toJSON (ValueFloat x) = toJSON x
  toJSON (ValueBoolean x) = toJSON x
  toJSON (ValueString x) = toJSON x
  toJSON (ValueEnum x) = toJSON x
  toJSON (ValueList x) = toJSON x
  toJSON (ValueMap x) = toJSON x

newtype String = String Text deriving (Eq, Ord, Show)

instance ToJSON String where
  toJSON (String x) = toJSON x

newtype List = List [Value] deriving (Eq, Ord, Show)

instance ToJSON List where
  toJSON (List x) = toJSON x

-- XXX: This is ObjectValue [ObjectField]; ObjectField Name Value upstream.
newtype Map = Map (Map.Map Name Value) deriving (Eq, Ord, Show)

instance ToJSON Map where
  toJSON (Map x) = toJSON x


data Field = Field Name Value deriving (Eq, Show, Ord)

makeField :: (StringConv name Name, ToValue value) => name -> value -> Field
makeField name value = Field (toS name) (toValue value)

data FieldSet = FieldSet (Set Field) deriving (Eq, Show)

instance ToValue FieldSet where
  toValue = toValue . fieldSetToMap

fieldSetToMap :: FieldSet -> Map
fieldSetToMap (FieldSet fields) = Map (Map.fromList [ (name, value) | Field name value <- toList fields ])

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
  toValue :: a -> Value

instance ToValue Value where
  toValue = identity

-- XXX: Should this just be for Foldable?
instance ToValue a => ToValue [a] where
  toValue = toValue . List . map toValue

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

instance (ToValue v) => ToValue (Map.Map Text v) where
  toValue = toValue . Map . map toValue

instance ToValue List where
  toValue = ValueList

instance ToValue Map where
  toValue = ValueMap

-- XXX: No "enum" instance because not sure what that would be in Haskell.
