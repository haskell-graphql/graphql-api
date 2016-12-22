{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Literal GraphQL values.
module GraphQL.Value
  ( Value(..)
  , toObject
  , ToValue(..)
  , valueToAST
  , Name
  , List
  , String(..)
    -- * Objects
  , Object
  , ObjectField(ObjectField)
    -- ** Constructing
  , makeObject
  , objectFromList
    -- ** Combining
  , unionObjects
    -- ** Querying
  , objectFields
  ) where

import Protolude

import Data.List.NonEmpty (NonEmpty)
import qualified Data.String
import Data.Aeson (ToJSON(..), (.=), pairs)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Test.QuickCheck (Arbitrary(..), oneof, listOf)

import GraphQL.Internal.AST (Name(..))
import qualified GraphQL.Internal.AST as AST

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

toObject :: Value -> Maybe Object
toObject (ValueObject o) = pure o
toObject _ = empty

instance ToJSON GraphQL.Value.Value where

  toJSON (ValueInt x) = toJSON x
  toJSON (ValueFloat x) = toJSON x
  toJSON (ValueBoolean x) = toJSON x
  toJSON (ValueString x) = toJSON x
  toJSON (ValueEnum x) = toJSON x
  toJSON (ValueList x) = toJSON x
  toJSON (ValueObject x) = toJSON x
  toJSON ValueNull = Aeson.Null

instance Arbitrary Value where
  arbitrary = oneof [ ValueInt <$> arbitrary
                    , ValueFloat <$> arbitrary
                    , ValueBoolean <$> arbitrary
                    , ValueString <$> arbitrary
                    , ValueEnum <$> arbitrary
                    , ValueList <$> arbitrary
                    , ValueObject <$> arbitrary
                    , pure ValueNull
                    ]

newtype String = String Text deriving (Eq, Ord, Show)

instance Arbitrary String where
  arbitrary = String . toS <$> arbitrary @Data.String.String

instance ToJSON String where
  toJSON (String x) = toJSON x

newtype List = List [Value] deriving (Eq, Ord, Show)

instance Arbitrary List where
  -- TODO: GraphQL does not allow heterogeneous lists:
  -- https://facebook.github.io/graphql/#sec-Lists, so this will generate
  -- invalid lists.
  arbitrary = List <$> listOf arbitrary

makeList :: (Functor f, Foldable f, ToValue a) => f a -> List
makeList = List . Protolude.toList . map toValue


instance ToJSON List where
  toJSON (List x) = toJSON x

-- | A literal GraphQL object.
--
-- Note that https://facebook.github.io/graphql/#sec-Response calls these
-- \"Maps\", but everywhere else in the spec refers to them as objects.
newtype Object = Object { objectFields :: [ObjectField] } deriving (Eq, Ord, Show)

instance Arbitrary Object where
  arbitrary = Object <$> fields
    where
      fields = zipWith ObjectField <$> names <*> values
      names = ordNub <$> listOf arbitrary
      values = listOf arbitrary


data ObjectField = ObjectField Name Value deriving (Eq, Ord, Show)

instance Arbitrary ObjectField where
  arbitrary = ObjectField <$> arbitrary <*> arbitrary

makeObject :: [ObjectField] -> Maybe Object
makeObject fields
  | fieldNames == ordNub fieldNames = pure (Object fields)
  | otherwise = empty
  where
    fieldNames = [name | ObjectField name _ <- fields]

objectFromList :: [(Name, Value)] -> Maybe Object
objectFromList = makeObject . map (uncurry ObjectField)

unionObjects :: [Object] -> Maybe Object
unionObjects objects = makeObject (objects >>= objectFields)

instance ToJSON Object where
  -- Direct encoding to preserve order of keys / values
  toJSON (Object xs) = toJSON (Map.fromList [(getNameText k, v) | ObjectField k v <- xs])
  toEncoding (Object xs) = pairs (foldMap (\(ObjectField k v) -> toS (getNameText k) .= v) xs)

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

-- | Convert a literal value into an AST value.
--
-- Nulls are converted into Nothing.
--
-- This function probably isn't particularly useful, but it functions as a
-- stop-gap until we have QuickCheck generators for the AST.
valueToAST :: Value -> Maybe AST.Value
valueToAST (ValueInt x) = pure $ AST.ValueInt x
valueToAST (ValueFloat x) = pure $ AST.ValueFloat x
valueToAST (ValueBoolean x) = pure $ AST.ValueBoolean x
valueToAST (ValueString (String x)) = pure $ AST.ValueString (AST.StringValue x)
valueToAST (ValueEnum x) = pure $ AST.ValueEnum x
valueToAST (ValueList (List xs)) = AST.ValueList . AST.ListValue <$> traverse valueToAST xs
valueToAST (ValueObject (Object fields)) = AST.ValueObject . AST.ObjectValue <$> traverse toObjectField fields
  where
    toObjectField (ObjectField name value) = AST.ObjectField name <$> valueToAST value
valueToAST ValueNull = empty
