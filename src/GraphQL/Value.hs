{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Literal GraphQL values.
module GraphQL.Value
  (
    Value(..)
  , ToValue(..)
  , Name
  , List
  , String
  , Object
  , objectFromList
  , unionObject
  ) where

import Protolude

import Data.List.NonEmpty (NonEmpty)
import Data.GraphQL.AST (Name)
import Data.Aeson (ToJSON(..), (.=), pairs)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map

-- | Concrete GraphQL value. Essentially Data.GraphQL.AST.Value, but without
-- the "variable" field.
data Value
  = ValueInt Int32
  | ValueFloat Double
  | ValueBoolean Bool
  | ValueString String
  | ValueEnum Name
  | ValueList List
  | ValueObject Object
  | ValueNull
  deriving (Eq, Ord, Show)

instance ToJSON GraphQL.Value.Value where

  toJSON (ValueInt x) = toJSON x
  toJSON (ValueFloat x) = toJSON x
  toJSON (ValueBoolean x) = toJSON x
  toJSON (ValueString x) = toJSON x
  toJSON (ValueEnum x) = toJSON x
  toJSON (ValueList x) = toJSON x
  toJSON (ValueObject x) = toJSON x
  toJSON ValueNull = Aeson.Null

newtype String = String Text deriving (Eq, Ord, Show)

instance ToJSON String where
  toJSON (String x) = toJSON x

newtype List = List [Value] deriving (Eq, Ord, Show)

makeList :: (Functor f, Foldable f, ToValue a) => f a -> List
makeList = List . toList . map toValue


instance ToJSON List where
  toJSON (List x) = toJSON x

-- | A literal GraphQL object.
--
-- Note that https://facebook.github.io/graphql/#sec-Response calls these
-- \"Maps\", but everywhere else in the spec refers to them as objects.
newtype Object = Object [ObjectField] deriving (Eq, Ord, Show, Monoid)

-- TODO: Property test for object that shows that Names are unique.

data ObjectField = ObjectField Name Value deriving (Eq, Ord, Show)

objectFromList :: [(Name, Value)] -> Object
objectFromList = Object . map (uncurry ObjectField)

-- TODO this would be nicer with a prism `_ValueMap` but don't want to
-- pull in lens as dependency.
-- TODO: actually figure out what the hell this is supposed to do.
-- TODO: Since there's only one failure, should probably be Maybe.
unionObject :: [Value] -> Either Text Value
unionObject values = map (ValueObject . fold) (traverse isValueMap values)
  where
    isValueMap = \case
      ValueObject m -> Right m
      _ -> Left "non-ValueObject member"


instance ToJSON Object where
  -- Direct encoding to preserve order of keys / values
  toJSON (Object xs) = toJSON (Map.fromList [(k, v) | ObjectField k v <- xs])
  toEncoding (Object xs) = pairs (foldMap (\(ObjectField k v) -> toS k .= v) xs)

-- | Turn a Haskell value into a GraphQL value.
class ToValue a where
  toValue :: a -> Value

instance ToValue Value where
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

instance ToValue Object where
  toValue = ValueObject
