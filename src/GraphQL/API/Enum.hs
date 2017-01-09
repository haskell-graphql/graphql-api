{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
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
import GraphQL.Internal.AST (Name, nameFromSymbol, NameError, formatNameError)
import GHC.Generics (D, (:+:)(..))
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage(..))

invalidEnumName :: forall t. NameError -> Either Text t
invalidEnumName x = Left ("In Enum: " <> formatNameError x)

class GenericEnumValues (f :: Type -> Type) where
  genericEnumValues :: [Either NameError Name]
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
  genericEnumValues = let name = nameFromSymbol @conName in name:(genericEnumValues @f)
  genericEnumFromValue vname =
    case nameFromSymbol @conName of
      Right name -> if name == vname
                    then L1 <$> Right (M1 U1)
                    else R1 <$> genericEnumFromValue vname
      Left x -> invalidEnumName x
  genericEnumToValue (L1 _) =
    case nameFromSymbol @conName of
      Right name -> name
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
  -- Sadly we need the Proxy to constain the type enough on the call
  -- site. TypeApplications fails here. This *might* be a GHC type
  -- resolution bug but I'm not confident enough to tell ATM.
  enumValues :: Proxy a -> [Either NameError Name]
  default enumValues :: (Generic a, GenericEnumValues (Rep a)) => Proxy a -> [Either NameError Name]
  enumValues _ = genericEnumValues @(Rep a)

  enumFromValue :: Name -> Either Text a
  default enumFromValue :: (Generic a, GenericEnumValues (Rep a)) => Name -> Either Text a
  enumFromValue v = to <$> genericEnumFromValue v

  enumToValue :: a -> Name
  default enumToValue :: (Generic a, GenericEnumValues (Rep a)) => a -> Name
  enumToValue = genericEnumToValue . from
