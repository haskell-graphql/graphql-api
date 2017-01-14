{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Literal GraphQL values.
module GraphQL.Value.FromValue
  ( FromValue(..)
  , prop_roundtripValue
  , wrongType
  ) where

import Protolude hiding (TypeError)
import GraphQL.Value
import GraphQL.Value.ToValue (ToValue(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics ((:*:)(..))
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage(..))

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
  default fromValue :: (Generic a, GenericFromValue (Rep a)) => Value' ConstScalar -> Either Text a
  fromValue v = to <$> genericFromValue v

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
wrongType expected value = throwError ("Wrong type, should be " <> expected <> show value)


-- We only allow generic record reading for now because I am not sure
-- how we should interpret any other generic things (e.g. tuples).
class GenericFromValue (f :: Type -> Type) where
  genericFromValue :: Value' ConstScalar -> Either Text (f p)

instance forall dataName consName records s l p.
  ( KnownSymbol dataName
  , KnownSymbol consName
  , GenericFromValue records
  ) => GenericFromValue (D1 ('MetaData dataName s l 'False)
                         (C1 ('MetaCons consName p 'True) records
                         )) where
  genericFromValue v = M1 . M1 <$> genericFromValue @records v

instance forall wrappedType fieldName rest u s l.
  ( KnownSymbol fieldName
  , FromValue wrappedType
  , GenericFromValue rest
  ) => GenericFromValue (S1 ('MetaSel ('Just fieldName) u s l) (Rec0 wrappedType) :*: rest) where
  genericFromValue v =
    let l = M1 . K1 <$> fromValue @wrappedType v
        r = genericFromValue @rest v
    in (:*:) <$> l <*> r

instance forall wrappedType fieldName u s l.
  ( KnownSymbol fieldName
  , FromValue wrappedType
  ) => GenericFromValue (S1 ('MetaSel ('Just fieldName) u s l) (Rec0 wrappedType)) where
  genericFromValue v = M1 . K1 <$> fromValue @wrappedType v

instance forall l r m.
  ( TypeError ('Text "Generic fromValue only works for records with exactly one data constructor.")
  ) => GenericFromValue (D1 m (l :+: r)) where
  genericFromValue = undefined
