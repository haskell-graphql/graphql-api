{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}

module GraphQL.API.Enum
  ( GraphQLEnum(..)
  ) where

import Protolude hiding (Enum, U1, TypeError)
import GraphQL.Internal.AST (Name, nameFromSymbol)
import qualified GraphQL.Value as GValue
import GHC.Generics (M1(..), Rep, Meta(..), C1, U1(..), D, (:+:)(..))
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage(..))


class GenricEnumValues (f :: Type -> Type) where
  genericEnumValues :: [Name]
  genericEnumFromValue :: GValue.Value -> Either Text (f p)
  genericEnumToValue :: f p -> GValue.Value

instance forall conName m p f nt.
  ( KnownSymbol conName
  , KnownSymbol m
  , KnownSymbol p
  , GenricEnumValues f
  ) => GenricEnumValues (M1 D ('MetaData conName m p nt) f) where
  genericEnumValues = genericEnumValues @f
  genericEnumFromValue v@(GValue.ValueEnum _) = M1 <$> genericEnumFromValue v
  genericEnumFromValue x = Left ("Not an enum: " <> show x)
  genericEnumToValue (M1 gv) = genericEnumToValue gv

instance forall conName f p b.
  ( KnownSymbol conName
  , GenricEnumValues f
  ) => GenricEnumValues (C1 ('MetaCons conName p b) U1 :+: f) where
  genericEnumValues = let Right name = nameFromSymbol @conName in name:(genericEnumValues @f)
  genericEnumFromValue v@(GValue.ValueEnum vname) =
    case nameFromSymbol @conName of
      Right name -> if name == vname
                    then L1 <$> Right (M1 U1)
                    else R1 <$> genericEnumFromValue v
      Left x -> Left ("Not a valid enum name: " <> show x)
  genericEnumFromValue _ = panic "This case should have been caught at top-level. Please file a bug."
  genericEnumToValue (L1 _) =
    let Right name = nameFromSymbol @conName
    in GValue.ValueEnum name
  genericEnumToValue (R1 gv) = genericEnumToValue gv

instance forall conName p b. (KnownSymbol conName) => GenricEnumValues (C1 ('MetaCons conName p b) U1) where
  genericEnumValues = let Right name = nameFromSymbol @conName in [name]
  genericEnumFromValue (GValue.ValueEnum vname) =
    case nameFromSymbol @conName of
      Right name -> if name == vname
                    then Right (M1 U1)
                    else Left ("Not a valid enum name: " <> show vname)
      Left x -> Left ("Not a valid enum name: " <> show x)
  genericEnumFromValue _ = panic "This case should have been caught at top-level. Please file a bug."
  genericEnumToValue (M1 _) =
    let Right name = nameFromSymbol @conName
    in GValue.ValueEnum name

-- TODO(tom): better type errors using `n`. Also type errors for other
-- invalid constructors.
instance forall conName p b sa sb.
  ( TypeError ('Text "Constructor not unary: " ':<>: 'Text conName)
  , KnownSymbol conName
  ) => GenricEnumValues (C1 ('MetaCons conName p b) (S1 sa sb)) where
  genericEnumValues = notImplemented
  genericEnumFromValue = notImplemented
  genericEnumToValue = notImplemented

instance forall conName p b sa sb f.
  ( TypeError ('Text "Constructor not unary: " ':<>: 'Text conName)
  , KnownSymbol conName
  ) => GenricEnumValues (C1 ('MetaCons conName p b) (S1 sa sb) :+: f) where
  genericEnumValues = notImplemented
  genericEnumFromValue = notImplemented
  genericEnumToValue = notImplemented


-- | For each enum type we need 1) a list of all possible values 2) a
-- way to serialise and 3) deserialise.
class GraphQLEnum a where
  -- Sadly we can't use visible type application to make @enumValues@
  -- a nullary function because the solver can't constrain the
  -- function signature without an @a@ argument. Hence the Proxy.
  enumValues :: Proxy a -> [Name]
  default enumValues :: (Generic a, GenricEnumValues (Rep a)) => Proxy a -> [Name]
  enumValues _ = genericEnumValues @(Rep a)

  enumFromValue :: GValue.Value -> Either Text a
  default enumFromValue :: (Generic a, GenricEnumValues (Rep a)) => GValue.Value -> Either Text a
  enumFromValue v = to <$> genericEnumFromValue v

  enumToValue :: a -> GValue.Value
  default enumToValue :: (Generic a, GenricEnumValues (Rep a)) => a -> GValue.Value
  enumToValue = genericEnumToValue . from
