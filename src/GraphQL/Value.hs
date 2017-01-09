{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Literal GraphQL values.
module GraphQL.Value
  ( Value
  , VariableValue
  , pattern ValueInt
  , pattern ValueFloat
  , pattern ValueBoolean
  , pattern ValueString
  , pattern ValueEnum
  , pattern ValueList
  , pattern ValueObject
  , pattern ValueNull
  , toObject
  , ToValue(..)
  , valueToAST
  , astToVariableValue
  , variableValueToAST
  , prop_roundtripValue
  , FromValue(..)
  , Name
  , List
  , String(..)
    -- * Objects
  , Object
  , ObjectField
  , ObjectField'(ObjectField)
    -- ** Constructing
  , makeObject
  , objectFromList
    -- ** Combining
  , unionObjects
    -- ** Querying
  , objectFields
  ) where

import Protolude

import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON(..), (.=), pairs)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, sized)

import GraphQL.Internal.Arbitrary (arbitraryText)
import GraphQL.Internal.AST (Name(..), Variable)
import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.OrderedMap (OrderedMap)
import qualified GraphQL.Internal.OrderedMap as OrderedMap


-- * Values

-- | A GraphQL value. @scalar@ represents the type of scalar that's contained
-- within this value.
--
-- Normally, it is one of either 'ConstScalar' (to indicate that there are no
-- variables whatsoever) or 'VariableScalar' (to indicate that there might be
-- some variables)
data Value' scalar
  = ValueScalar' scalar
  | ValueList' (List' scalar)
  | ValueObject' (Object' scalar)
  deriving (Eq, Ord, Show, Functor)

instance Foldable Value' where
  foldMap f (ValueScalar' scalar) = f scalar
  foldMap f (ValueList' (List' values)) = mconcat (map (foldMap f) values)
  foldMap f (ValueObject' (Object' fieldMap)) = foldMap (foldMap f) fieldMap

instance Traversable Value' where
  traverse f (ValueScalar' x) = ValueScalar' <$> f x
  traverse f (ValueList' (List' xs)) = ValueList' . List' <$> traverse (traverse f) xs
  traverse f (ValueObject' (Object' xs)) = ValueObject' . Object' <$> traverse (traverse f) xs

instance ToJSON scalar => ToJSON (Value' scalar) where
  toJSON (ValueScalar' x) = toJSON x
  toJSON (ValueList' x) = toJSON x
  toJSON (ValueObject' x) = toJSON x

instance Arbitrary scalar => Arbitrary (Value' scalar) where
  -- | Generate an arbitrary value. Uses the generator's \"size\" property to
  -- determine maximum object depth.
  arbitrary = sized genValue

-- | Generate an arbitrary value, with objects at most @n@ levels deep.
genValue :: Arbitrary scalar => Int -> Gen (Value' scalar)
genValue n
  | n <= 0 = arbitrary
  | otherwise = oneof [ ValueScalar' <$> arbitrary
                      , ValueObject' <$> genObject (n - 1)
                      , ValueList' . List' <$> listOf (genValue (n - 1))
                      ]

-- | A GraphQL value which contains no variables.
type Value = Value' ConstScalar

-- | A GraphQL value which might contain some variables.
type VariableValue = Value' VariableScalar

pattern ValueInt :: Int32 -> Value
pattern ValueInt x = ValueScalar' (ConstInt x)

pattern ValueFloat :: Double -> Value
pattern ValueFloat x = ValueScalar' (ConstFloat x)

pattern ValueBoolean :: Bool -> Value
pattern ValueBoolean x = ValueScalar' (ConstBoolean x)

pattern ValueString :: String -> Value
pattern ValueString x = ValueScalar' (ConstString x)

pattern ValueEnum :: Name -> Value
pattern ValueEnum x = ValueScalar' (ConstEnum x)

pattern ValueList :: forall t. List' t -> Value' t
pattern ValueList x = ValueList' x

pattern ValueObject :: forall t. Object' t -> Value' t
pattern ValueObject x = ValueObject' x

pattern ValueNull :: Value
pattern ValueNull = ValueScalar' ConstNull

-- | If a value is an object, return just that. Otherwise @Nothing@.
toObject :: Value' scalar -> Maybe (Object' scalar)
toObject (ValueObject' o) = pure o
toObject _ = empty

-- * Scalars

-- | A non-variable value which contains no other values.
data ConstScalar
  = ConstInt Int32
  | ConstFloat Double
  | ConstBoolean Bool
  | ConstString String
  | ConstEnum Name
  | ConstNull
  deriving (Eq, Ord, Show)

instance ToJSON ConstScalar where
  toJSON (ConstInt x) = toJSON x
  toJSON (ConstFloat x) = toJSON x
  toJSON (ConstBoolean x) = toJSON x
  toJSON (ConstString x) = toJSON x
  toJSON (ConstEnum x) = toJSON x
  toJSON ConstNull = Aeson.Null

-- | A value which contains no other values, and might be a variable.
type VariableScalar = Either Variable ConstScalar

-- | Generate an arbitrary scalar value.
instance Arbitrary ConstScalar where
  arbitrary = oneof [ ConstInt <$> arbitrary
                    , ConstFloat <$> arbitrary
                    , ConstBoolean <$> arbitrary
                    , ConstString <$> arbitrary
                    , ConstEnum <$> arbitrary
                    , pure ConstNull
                    ]

-- | Convert a constant scalar to an AST.Value
constToAST :: ConstScalar -> AST.Value
constToAST scalar =
  case scalar of
    ConstInt x -> AST.ValueInt x
    ConstFloat x -> AST.ValueFloat x
    ConstBoolean x -> AST.ValueBoolean x
    ConstString (String x) -> AST.ValueString (AST.StringValue x)
    ConstEnum x -> AST.ValueEnum x
    ConstNull -> AST.ValueNull

-- | Convert a variable scalar to an AST.Value
variableToAST :: VariableScalar -> AST.Value
variableToAST (Left variable) = AST.ValueVariable variable
variableToAST (Right constant) = constToAST constant

-- | Convert a value from the AST into a variable scalar, presuming it /is/ a
-- scalar.
astToScalar :: AST.Value -> Maybe VariableScalar
astToScalar (AST.ValueInt x) = pure $ Right $ ConstInt x
astToScalar (AST.ValueFloat x) = pure $ Right $ ConstFloat x
astToScalar (AST.ValueBoolean x) = pure $ Right $ ConstBoolean x
astToScalar (AST.ValueString (AST.StringValue x)) = pure $ Right $ ConstString (String x)
astToScalar (AST.ValueEnum x) = pure $ Right $ ConstEnum x
astToScalar AST.ValueNull = pure $ Right ConstNull
astToScalar (AST.ValueVariable x) = pure $ Left x
astToScalar _ = empty


-- * Strings

newtype String = String Text deriving (Eq, Ord, Show)

instance Arbitrary String where
  arbitrary = String <$> arbitraryText

instance ToJSON String where
  toJSON (String x) = toJSON x

-- * Lists

newtype List' scalar = List' [Value' scalar] deriving (Eq, Ord, Show, Functor)

-- | A list of values that are known to be constants.
--
-- Note that this list might not be valid GraphQL, because GraphQL only allows
-- homogeneous lists (i.e. all elements of the same type), and we do no type
-- checking at this point.
type List = List' ConstScalar

instance Arbitrary scalar => Arbitrary (List' scalar) where
  -- TODO: GraphQL does not allow heterogeneous lists:
  -- https://facebook.github.io/graphql/#sec-Lists, so this will generate
  -- invalid lists.
  arbitrary = List' <$> listOf arbitrary

makeList :: (Functor f, Foldable f, ToValue a) => f a -> List
makeList = List' . Protolude.toList . map toValue


instance ToJSON scalar => ToJSON (List' scalar) where
  toJSON (List' x) = toJSON x

-- * Objects

-- | A GraphQL object.
--
-- Note that https://facebook.github.io/graphql/#sec-Response calls these
-- \"Maps\", but everywhere else in the spec refers to them as objects.
newtype Object' scalar = Object' (OrderedMap Name (Value' scalar)) deriving (Eq, Ord, Show, Functor)

-- | A GraphQL object that contains only non-variable values.
type Object = Object' ConstScalar

objectFields :: Object' scalar -> [ObjectField' scalar]
objectFields (Object' object) = map (uncurry ObjectField') (OrderedMap.toList object)

instance Arbitrary scalar => Arbitrary (Object' scalar) where
  arbitrary = sized genObject

-- | Generate an arbitrary object to the given maximum depth.
genObject :: Arbitrary scalar => Int -> Gen (Object' scalar)
genObject n = Object' <$> OrderedMap.genOrderedMap arbitrary (genValue n)

data ObjectField' scalar = ObjectField' Name (Value' scalar) deriving (Eq, Ord, Show, Functor)

-- | A field of an object that has a non-variable value.
type ObjectField = ObjectField' ConstScalar

pattern ObjectField :: forall t. Name -> Value' t -> ObjectField' t
pattern ObjectField name value = ObjectField' name value

instance Arbitrary scalar => Arbitrary (ObjectField' scalar) where
  arbitrary = ObjectField' <$> arbitrary <*> arbitrary

-- | Make an object from a list of object fields.
makeObject :: [ObjectField' scalar] -> Maybe (Object' scalar)
makeObject fields = objectFromList [(name, value) | ObjectField' name value <- fields]

-- | Create an object from a list of (name, value) pairs.
objectFromList :: [(Name, Value' scalar)] -> Maybe (Object' scalar)
objectFromList xs = Object' <$> OrderedMap.orderedMap xs

unionObjects :: [Object' scalar] -> Maybe (Object' scalar)
unionObjects objects = Object' <$> OrderedMap.unions [obj | Object' obj <- objects]

instance ToJSON scalar => ToJSON (Object' scalar) where
  -- Direct encoding to preserve order of keys / values
  toJSON (Object' xs) = toJSON (Map.fromList [(getNameText k, v) | (k, v) <- OrderedMap.toList xs])
  toEncoding (Object' xs) = pairs (foldMap (\(k, v) -> toS (getNameText k) .= v) (OrderedMap.toList xs))


-- * ToValue

-- | Turn a Haskell value into a GraphQL value.
class ToValue a where
  toValue :: a -> Value' ConstScalar

instance ToValue (Value' ConstScalar) where
  toValue = identity

-- XXX: Should this just be for Foldable?
instance ToValue a => ToValue [a] where
  toValue = toValue . List' . map toValue

instance ToValue a => ToValue (NonEmpty a) where
  toValue = toValue . makeList

instance ToValue Bool where
  toValue = ValueScalar' . ConstBoolean

instance ToValue Int32 where
  toValue = ValueScalar' . ConstInt

instance ToValue Double where
  toValue = ValueScalar' . ConstFloat

instance ToValue String where
  toValue = ValueScalar' . ConstString

-- XXX: Make more generic: any string-like thing rather than just Text.
instance ToValue Text where
  toValue = toValue . String

instance ToValue List where
  toValue = ValueList'

instance ToValue (Object' ConstScalar) where
  toValue = ValueObject'

-- * FromValue

-- | @a@ can be converted from a GraphQL 'Value' to a Haskell value.
--
-- The @FromValue@ instance converts 'AST.Value' to the type expected by the
-- handler function. It is the boundary between incoming data and your custom
-- application Haskell types.
class FromValue a where
  -- | Convert an already-parsed value into a Haskell value, generally to be
  -- passed to a handler.
  fromValue :: Value' ConstScalar -> Either Text a

instance FromValue Int32 where
  fromValue (ValueScalar' (ConstInt v)) = pure v
  fromValue v = wrongType "Int" v

instance FromValue Double where
  fromValue (ValueScalar' (ConstFloat v)) = pure v
  fromValue v = wrongType "Double" v

instance FromValue Bool where
  fromValue (ValueScalar' (ConstBoolean v)) = pure v
  fromValue v = wrongType "Bool" v

instance FromValue Text where
  fromValue (ValueScalar' (ConstString (String v))) = pure v
  fromValue v = wrongType "String" v

instance forall v. FromValue v => FromValue [v] where
  fromValue (ValueList' (List' values)) = traverse (fromValue @v) values
  fromValue v = wrongType "List" v

instance forall v. FromValue v => FromValue (NonEmpty v) where
  fromValue (ValueList' (List' values)) =
    case NonEmpty.nonEmpty values of
      Nothing -> Left "Cannot construct NonEmpty from empty list"
      Just values' -> traverse (fromValue @v) values'
  fromValue v = wrongType "List" v

instance forall v. FromValue v => FromValue (Maybe v) where
  fromValue (ValueScalar' ConstNull) = pure Nothing
  fromValue x = Just <$> fromValue @v x

-- | Anything that can be converted to a value and from a value should roundtrip.
prop_roundtripValue :: forall a. (Eq a, ToValue a, FromValue a) => a -> Bool
prop_roundtripValue x = fromValue (toValue x) == Right x

-- | Throw an error saying that @value@ does not have the @expected@ type.
wrongType :: (MonadError Text m, Show a) => Text -> a -> m b
wrongType expected value = throwError ("Wrong type, should be " <> expected <> show value)

-- * Conversion to and from AST.

-- | Convert an AST value into a literal value.
--
-- This is a stop-gap until we have proper conversion of user queries into
-- canonical forms.
astToValue' :: (AST.Value -> scalar) -> AST.Value -> Maybe (Value' scalar)
astToValue' f x@(AST.ValueInt _) = pure (ValueScalar' (f x))
astToValue' f x@(AST.ValueFloat _) = pure (ValueScalar' (f x))
astToValue' f x@(AST.ValueBoolean _) = pure (ValueScalar' (f x))
astToValue' f x@(AST.ValueString (AST.StringValue _)) = pure (ValueScalar' (f x))
astToValue' f x@(AST.ValueEnum _) = pure (ValueScalar' (f x))
astToValue' f AST.ValueNull = pure (ValueScalar' (f AST.ValueNull))
astToValue' f x@(AST.ValueVariable _) = pure (ValueScalar' (f x))
astToValue' f (AST.ValueList (AST.ListValue xs)) = ValueList' . List' <$> traverse (astToValue' f) xs
astToValue' f (AST.ValueObject (AST.ObjectValue fields)) = do
  fields' <- traverse toObjectField fields
  object <- makeObject fields'
  pure (ValueObject' object)
  where
    toObjectField (AST.ObjectField name value) = ObjectField' name <$> astToValue' f value

-- | Convert an AST value to a variable value.
--
-- Will fail if the AST value contains duplicate object fields, or is
-- otherwise invalid.
astToVariableValue :: HasCallStack => AST.Value -> Maybe VariableValue
astToVariableValue ast = astToValue' convertScalar ast
  where
    convertScalar x =
      case astToScalar x of
        Just scalar -> scalar
        Nothing -> panic ("Non-scalar passed to convertScalar, bug in astToValue': " <> show x)

-- | Convert a value to an AST value.
valueToAST :: Value -> AST.Value
valueToAST = valueToAST' constToAST

-- | Convert a variable value to an AST value.
variableValueToAST :: VariableValue -> AST.Value
variableValueToAST = valueToAST' variableToAST

-- | Convert a literal value into an AST value.
--
-- Nulls are converted into Nothing.
--
-- This function probably isn't particularly useful, but it functions as a
-- stop-gap until we have QuickCheck generators for the AST.
valueToAST' :: (scalar -> AST.Value) -> Value' scalar -> AST.Value
valueToAST' f (ValueScalar' x) = f x
valueToAST' f (ValueList' (List' xs)) = AST.ValueList (AST.ListValue (map (valueToAST' f) xs))
valueToAST' f (ValueObject' (Object' fields)) = AST.ValueObject (AST.ObjectValue (map toObjectField (OrderedMap.toList fields)))
  where
    toObjectField (name, value) = AST.ObjectField name (valueToAST' f value)
