{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Define GraphQL Enums with Haskell types
module GraphQL.Internal.API.Enum
  ( GraphQLEnum(..)
  ) where

import Protolude hiding (Enum, TypeError)

import GHC.Generics (D, (:+:)(..))
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage(..))
import GHC.Types (Type)

import GraphQL.Internal.Name (Name, nameFromSymbol, NameError)
import GraphQL.Internal.Output (GraphQLError(..))

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

instance forall left right.
  ( GenericEnumValues left
  , GenericEnumValues right
  ) => GenericEnumValues (left :+: right) where
  genericEnumValues = genericEnumValues @left <> genericEnumValues @right
  genericEnumFromValue vname =
    let left = genericEnumFromValue @left vname
        right = genericEnumFromValue @right vname
    in case (left, right) of
      (x@(Right _), Left _) -> L1 <$> x
      (Left _, x@(Right _)) -> R1 <$> x
      (err@(Left _), Left _) -> L1 <$> err
      _ -> panic "Can't have two successful branches in Haskell"

  genericEnumToValue (L1 gv) = genericEnumToValue gv
  genericEnumToValue (R1 gv) = genericEnumToValue gv

instance forall conName p b. (KnownSymbol conName) => GenericEnumValues (C1 ('MetaCons conName p b) U1) where
  genericEnumValues = let name = nameFromSymbol @conName in [name]
  genericEnumFromValue vname =
    case nameFromSymbol @conName of
      Right name -> if name == vname
                    then Right (M1 U1)
                    else Left ("Not a valid choice for enum: " <> show vname)
      -- XXX: This is impossible to catch during validation, because we cannot
      -- validate type-level symbols, we can only validate values. We could
      -- show that the schema is invalid at the type-level and still decide to
      -- call this anyway. The error should rather say that the schema is
      -- invalid.
      --
      -- Further, we don't actually have any schema-level validation, so
      -- "should have been caught during validation" is misleading.
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
  genericEnumValues = nonUnaryConstructorError
  genericEnumFromValue = nonUnaryConstructorError
  genericEnumToValue = nonUnaryConstructorError

instance forall conName p b sa sb f.
  ( TypeError ('Text "Constructor not unary: " ':<>: 'Text conName)
  , KnownSymbol conName
  ) => GenericEnumValues (C1 ('MetaCons conName p b) (S1 sa sb) :+: f) where
  genericEnumValues = nonUnaryConstructorError
  genericEnumFromValue = nonUnaryConstructorError
  genericEnumToValue = nonUnaryConstructorError

nonUnaryConstructorError :: a
nonUnaryConstructorError = panic "Tried to construct enum with non-unary constructor. Should get a compile-time error instead of this."

-- | For each enum type we need 1) a list of all possible values 2) a
-- way to serialise and 3) deserialise.
--
-- TODO: Update this comment to explain what a GraphQLEnum is, why you might
-- want an instance, and any laws that apply to method relations.
class GraphQLEnum a where
  -- TODO: Document each of these methods.
  enumValues :: [Either NameError Name]
  default enumValues :: (Generic a, GenericEnumValues (Rep a)) => [Either NameError Name]
  enumValues = genericEnumValues @(Rep a)

  enumFromValue :: Name -> Either Text a
  default enumFromValue :: (Generic a, GenericEnumValues (Rep a)) => Name -> Either Text a
  enumFromValue v = to <$> genericEnumFromValue v

  enumToValue :: a -> Name
  default enumToValue :: (Generic a, GenericEnumValues (Rep a)) => a -> Name
  enumToValue = genericEnumToValue . from
