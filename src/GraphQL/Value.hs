{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Literal GraphQL values.
module GraphQL.Value
  ( Value(..)
  , toObject
  , ToValue(..)
  , Name
  , makeName
  , unsafeMakeName
  , List
  , String
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
import Data.Aeson (ToJSON(..), (.=), pairs)
import qualified Data.Aeson as Aeson
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Map as Map
import qualified Data.GraphQL.Parser as Parser

-- | A name in GraphQL.
--
-- https://facebook.github.io/graphql/#sec-Names
newtype Name = Name { getName :: Text } deriving (Eq, Ord, Show)

instance ToJSON Name where
  toJSON = toJSON . getName

-- | Create a 'Name'.
--
-- Names must match the regex @[_A-Za-z][_0-9A-Za-z]*@. If the given text does
-- not match, return Nothing.
--
-- >>> makeName "foo"
-- Just (Name {getName = "foo"})
-- >>> makeName "9-bar"
-- Nothing
makeName :: Text -> Maybe Name
makeName = map Name . hush . parseOnly Parser.name

-- | Create a 'Name', panicking if the given text is invalid.
--
-- Prefer 'makeName' to this in all cases.
--
-- >>> unsafeMakeName "foo"
-- Name {getName = "foo"}
unsafeMakeName :: Text -> Name
unsafeMakeName name = fromMaybe (panic $ "Not a valid GraphQL name: " <> show name) (makeName name)

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

newtype String = String Text deriving (Eq, Ord, Show)

instance ToJSON String where
  toJSON (String x) = toJSON x

newtype List = List [Value] deriving (Eq, Ord, Show)

makeList :: (Functor f, Foldable f, ToValue a) => f a -> List
makeList = List . Protolude.toList . map toValue


instance ToJSON List where
  toJSON (List x) = toJSON x

-- | A literal GraphQL object.
--
-- Note that https://facebook.github.io/graphql/#sec-Response calls these
-- \"Maps\", but everywhere else in the spec refers to them as objects.
newtype Object = Object { objectFields :: [ObjectField] } deriving (Eq, Ord, Show)

data ObjectField = ObjectField Name Value deriving (Eq, Ord, Show)

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
  toJSON (Object xs) = toJSON (Map.fromList [(getName k, v) | ObjectField k v <- xs])
  toEncoding (Object xs) = pairs (foldMap (\(ObjectField k v) -> toS (getName k) .= v) xs)

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
