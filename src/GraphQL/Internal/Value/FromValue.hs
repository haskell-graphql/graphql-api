{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Convert GraphQL values to domain-specific Haskell values
module GraphQL.Internal.Value.FromValue
  ( FromValue(..)
  , prop_roundtripValue
  , wrongType
  ) where

import Protolude hiding (TypeError)

import qualified Data.List.NonEmpty as NonEmpty
import GHC.Generics ((:*:)(..))
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage(..))
import GHC.Types (Type)

import GraphQL.Internal.Name (nameFromSymbol)
import qualified GraphQL.Internal.OrderedMap as OM
import GraphQL.Internal.Value
import GraphQL.Internal.Value.ToValue (ToValue(..))

-- * FromValue

-- | @a@ can be converted from a GraphQL 'Value' to a Haskell value.
--
-- The @FromValue@ instance converts 'AST.Value' to the type expected by the
-- handler function. It is the boundary between incoming data and your custom
-- application Haskell types.
--
-- @FromValue@ has a generic instance for converting input objects to
-- records.
class FromValue a where
  -- | Convert an already-parsed value into a Haskell value, generally to be
  -- passed to a handler.
  fromValue :: Value' ConstScalar -> Either Text a
  default fromValue :: (Generic a, GenericFromValue (Rep a)) => Value' ConstScalar -> Either Text a
  fromValue (ValueObject v) = to <$> genericFromValue v
  fromValue v = wrongType "genericFromValue only works with objects." v

instance FromValue Int32 where
  fromValue (ValueInt v) = pure v
  fromValue v = wrongType "Int" v

instance FromValue Double where
  fromValue (ValueFloat v) = pure v
  fromValue v = wrongType "Double" v

instance FromValue Bool where
  fromValue (ValueBoolean v) = pure v
  fromValue v = wrongType "Bool" v

instance FromValue Text where
  fromValue (ValueString (String v)) = pure v
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
  fromValue ValueNull = pure Nothing
  fromValue x = Just <$> fromValue @v x

-- | Anything that can be converted to a value and from a value should roundtrip.
prop_roundtripValue :: forall a. (Eq a, ToValue a, FromValue a) => a -> Bool
prop_roundtripValue x = fromValue (toValue x) == Right x

-- | Throw an error saying that @value@ does not have the @expected@ type.
wrongType :: (MonadError Text m, Show a) => Text -> a -> m b
wrongType expected value = throwError ("Wrong type, should be: `" <> expected <> "` but is: `" <> show value <> "`")

-- We only allow generic record reading for now because I am not sure
-- how we should interpret any other generic things (e.g. tuples).
class GenericFromValue (f :: Type -> Type) where
  genericFromValue :: Object' ConstScalar -> Either Text (f p)

instance forall dataName consName records s l p.
  ( KnownSymbol dataName
  , KnownSymbol consName
  , GenericFromValue records
  ) => GenericFromValue (D1 ('MetaData dataName s l 'False)
                         (C1 ('MetaCons consName p 'True) records
                         )) where
  genericFromValue o = M1 . M1 <$> genericFromValue @records o


instance forall l r.
  ( GenericFromValue l
  , GenericFromValue r
  ) => GenericFromValue (l :*: r) where
  genericFromValue object = liftA2 (:*:) (genericFromValue @l object) (genericFromValue @r object)

-- | Look up a single record field element in the Object.
getValue :: forall wrappedType fieldName u s l p. (FromValue wrappedType, KnownSymbol fieldName)
         => Object' ConstScalar -> Either Text ((S1 ('MetaSel ('Just fieldName) u s l) (Rec0 wrappedType)) p)
getValue (Object' fieldMap) = do
  fieldName <- case nameFromSymbol @fieldName of
    Left err -> throwError ("invalid field name" <> show err)
    Right name' -> pure name'
   -- TODO(tom): How do we deal with optional fields? Maybe sounds
   -- like the correct type, but how would Maybe be different from
   -- `null`? Delegating to FromValue not good enough here because of
   -- the dictionary lookup.
  case OM.lookup fieldName fieldMap of
    Nothing -> throwError ("Key not found: " <> show fieldName)
    Just v -> M1 . K1 <$> fromValue @wrappedType v

instance forall wrappedType fieldName u s l.
  ( KnownSymbol fieldName
  , FromValue wrappedType
  ) => GenericFromValue (S1 ('MetaSel ('Just fieldName) u s l) (Rec0 wrappedType)) where
  genericFromValue = getValue @wrappedType @fieldName

instance forall l r m.
  ( TypeError ('Text "Generic fromValue only works for records with exactly one data constructor.")
  ) => GenericFromValue (D1 m (l :+: r)) where
  genericFromValue = panic "genericFromValue cannot be called for records with more than one data constructor. Code that tries will not be compiled."
