{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphQL.API.Enum
  ( GraphQLEnum(..)
  ) where

import Protolude hiding (Enum, TypeError)
import GraphQL.Internal.Name (Name, nameFromSymbol, NameError)
import GraphQL.Internal.Output (GraphQLError(..))
import GHC.Generics (D, (:+:)(..))
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage(..))

invalidEnumName :: forall t. NameError -> Either Text t
invalidEnumName x = Left ("In Enum: " <> formatError x)

-- TODO: Enums have a slightly more restricted set of names than 'Name'
-- implies. Especially, they cannot be 'true', 'false', or 'nil'. The parser
-- /probably/ guarantees this, so it should export this guarantee by providing
-- an 'Enum' type.

class GenericEnumValues (f :: Type -> Type) where
  genericEnumValues :: [Either NameError Name]
  -- XXX: Why is this 'Text' and not 'NameError'?
  genericEnumFromValue :: Name -> Either Text (f p)
  genericEnumToValue :: f p -> Name

instance forall conName m p f nt.
  ( KnownSymbol conName
  , KnownSymbol m
  , KnownSymbol p
  , GenericEnumValues f
  ) => GenericEnumValues (M1 D ('MetaData conName m p nt) f) where
  genericEnumValues = genericEnumValues @f
  genericEnumFromValue name = M1 <$> genericEnumFromValue name
  genericEnumToValue (M1 gv) = genericEnumToValue gv

instance forall conName f p b.
  ( KnownSymbol conName
  , GenericEnumValues f
  ) => GenericEnumValues (C1 ('MetaCons conName p b) U1 :+: f) where
  genericEnumValues = let name = nameFromSymbol @conName in name:genericEnumValues @f
  genericEnumFromValue vname =
    case nameFromSymbol @conName of
      Right name -> if name == vname
                    then L1 <$> Right (M1 U1)
                    else R1 <$> genericEnumFromValue vname
      Left x -> invalidEnumName x
  genericEnumToValue (L1 _) =
    case nameFromSymbol @conName of
      Right name -> name
      -- XXX: This is impossible to catch during validation, because we cannot
      -- validate type-level symbols, we can only validate values. We could
      -- show that the schema is invalid at the type-level and still decide to
      -- call this anyway. The error should rather say that the schema is
      -- invalid.
      --
      -- Further, we don't actually have any schema-level validation, so
      -- "should have been caught during validation" is misleading.
      Left err -> panic ("Invalid name: " <> show err <> ". This should have been caught during validation. Please file a bug.")
  genericEnumToValue (R1 gv) = genericEnumToValue gv

instance forall conName p b. (KnownSymbol conName) => GenericEnumValues (C1 ('MetaCons conName p b) U1) where
  genericEnumValues = let name = nameFromSymbol @conName in [name]
  genericEnumFromValue vname =
    case nameFromSymbol @conName of
      Right name -> if name == vname
                    then Right (M1 U1)
                    else Left ("Not a valid choice for enum: " <> show vname)
      Left x -> invalidEnumName x
  genericEnumToValue (M1 _) =
    let Right name = nameFromSymbol @conName
    in name

-- TODO(tom): better type errors using `n`. Also type errors for other
-- invalid constructors.
instance forall conName p b sa sb.
  ( TypeError ('Text "Constructor not unary: " ':<>: 'Text conName)
  , KnownSymbol conName
  ) => GenericEnumValues (C1 ('MetaCons conName p b) (S1 sa sb)) where
  genericEnumValues = undefined
  genericEnumFromValue = undefined
  genericEnumToValue = undefined

instance forall conName p b sa sb f.
  ( TypeError ('Text "Constructor not unary: " ':<>: 'Text conName)
  , KnownSymbol conName
  ) => GenericEnumValues (C1 ('MetaCons conName p b) (S1 sa sb) :+: f) where
  genericEnumValues = undefined
  genericEnumFromValue = undefined
  genericEnumToValue = undefined


-- | For each enum type we need 1) a list of all possible values 2) a
-- way to serialise and 3) deserialise.
class GraphQLEnum a where
  enumValues :: [Either NameError Name]
  default enumValues :: (Generic a, GenericEnumValues (Rep a)) => [Either NameError Name]
  enumValues = genericEnumValues @(Rep a)

  enumFromValue :: Name -> Either Text a
  default enumFromValue :: (Generic a, GenericEnumValues (Rep a)) => Name -> Either Text a
  enumFromValue v = to <$> genericEnumFromValue v

  enumToValue :: a -> Name
  default enumToValue :: (Generic a, GenericEnumValues (Rep a)) => a -> Name
  enumToValue = genericEnumToValue . from
