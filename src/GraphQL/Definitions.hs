{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeFamilyDependencies #-}
{-# LANGUAGE GADTs, AllowAmbiguousTypes, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeApplications, TypeInType #-}
{-# LANGUAGE OverloadedLabels, MagicHash #-}

-- | Type-level definitions for a GraphQL schema.
module GraphQL.Definitions where

import Protolude hiding (Enum)

import GraphQL.Schema hiding (Type)
import qualified GraphQL.Schema (Type)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified GHC.TypeLits (TypeError, ErrorMessage(..))
import qualified GraphQL.Value as GValue
import qualified Data.GraphQL.AST as AST

-- | Argument operator.
data a :> b = a :> b
infixr 8 :>

-- | Object result operator.
data a :<> b = a :<> b
infixr 8 :<>


data Object (name :: Symbol) (interfaces :: [Type]) (fields :: [Type])
data Enum (name :: Symbol) (values :: Type)
data Union (name :: Symbol) (types :: [Type])
data List (elemType :: Type)

-- TODO(tom): AFACIT We can't constrain "fields" to e.g. have at least
-- one field in it - is this a problem?
data Interface (name :: Symbol) (fields :: [Type])
data Field (name :: Symbol) (fieldType :: Type)
data Argument (name :: Symbol) (argType :: Type)


-- Can't set the value for default arguments via types, but can
-- distinguish to force users to provide a default argument somewhere
-- in their function (using Maybe? ore some new type like
-- https://hackage.haskell.org/package/optional-args-1.0.1)
data DefaultArgument (name :: Symbol) (argType :: Type)


-- Transform into a Schema definition
class HasObjectDefinition a where
  -- Todo rename to getObjectTypeDefinition
  getDefinition :: ObjectTypeDefinition

class HasFieldDefinition a where
  getFieldDefinition :: FieldDefinition


-- Fields
class HasFieldDefinitions a where
  getFieldDefinitions :: [FieldDefinition]

instance forall a as. (HasFieldDefinition a, HasFieldDefinitions as) => HasFieldDefinitions (a:as) where
  getFieldDefinitions = (getFieldDefinition @a):(getFieldDefinitions @as)

instance HasFieldDefinitions '[] where
  getFieldDefinitions = []

-- | For each enum type we need 1) a list of all possible values 2) a
-- way to serialise and 3) deserialise.
class GraphQLEnum a where
  enumValues :: [AST.Name]
  enumFromValue :: GValue.Value -> Either Text a
  enumToValue :: a -> GValue.Value
  -- TODO: These are trivially generically derivable

-- object types from union type lists, e.g. for
-- Union "Horse" '[Leg, Head, Tail]
--               ^^^^^^^^^^^^^^^^^^ this part
class UnionTypeObjectTypeDefinitionList a where
  getUnionTypeObjectTypeDefinitions :: [ObjectTypeDefinition]

instance forall a as. (HasObjectDefinition a, UnionTypeObjectTypeDefinitionList as) => UnionTypeObjectTypeDefinitionList (a:as) where
  getUnionTypeObjectTypeDefinitions = (getDefinition @a):(getUnionTypeObjectTypeDefinitions @as)

instance UnionTypeObjectTypeDefinitionList '[] where
  getUnionTypeObjectTypeDefinitions = []


-- Interfaces
class HasInterfaceDefinitions a where
  getInterfaceDefinitions :: Interfaces

instance forall a as. (HasInterfaceDefinition a, HasInterfaceDefinitions as) => HasInterfaceDefinitions (a:as) where
  getInterfaceDefinitions = (getInterfaceDefinition @a):(getInterfaceDefinitions @as)

instance HasInterfaceDefinitions '[] where
  getInterfaceDefinitions = []

class HasInterfaceDefinition a where
  getInterfaceDefinition :: InterfaceTypeDefinition

instance forall ks fields. (KnownSymbol ks, HasFieldDefinitions fields) => HasInterfaceDefinition (Interface ks fields) where
  getInterfaceDefinition =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        in InterfaceTypeDefinition name (NonEmptyList (getFieldDefinitions @fields))

-- Give users some help if they don't terminate Arguments with a Field:
-- NB the "redundant constraints" warning is a GHC bug: https://ghc.haskell.org/trac/ghc/ticket/11099
instance forall ks t. GHC.TypeLits.TypeError ('GHC.TypeLits.Text ":> Arguments must end with a Field") =>
         HasFieldDefinition (Argument ks t) where
  getFieldDefinition = notImplemented

instance forall ks is ts. (KnownSymbol ks, HasInterfaceDefinitions is, HasFieldDefinitions ts) => HasAnnotatedType (Object ks is ts) where
  getAnnotatedType =
    let obj = getDefinition @(Object ks is ts)
    in (TypeNamed . DefinedType . TypeDefinitionObject) obj

instance forall t ks. (KnownSymbol ks, HasAnnotatedType t) => HasFieldDefinition (Field ks t) where
  getFieldDefinition =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
    in FieldDefinition name [] (getAnnotatedType @t)


instance forall ks t b. (KnownSymbol ks, HasAnnotatedInputType t, HasFieldDefinition b) => HasFieldDefinition ((Argument ks t) :> b) where
  getFieldDefinition =
    let (FieldDefinition name argDefs at) = getFieldDefinition @b
        argName = Name (toS (symbolVal (Proxy :: Proxy ks)))
        arg = ArgumentDefinition argName (getAnnotatedInputType @t) Nothing
    in (FieldDefinition name (arg:argDefs) at)


instance forall ks is fields.
  (KnownSymbol ks, HasInterfaceDefinitions is, HasFieldDefinitions fields) =>
  HasObjectDefinition (Object ks is fields) where
  getDefinition =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
    in ObjectTypeDefinition name (getInterfaceDefinitions @is) (NonEmptyList (getFieldDefinitions @fields))

-- Builtin output types (annotated types)
class HasAnnotatedType a where
  -- TODO - the fact that we have to return TypeNonNull for normal
  -- types will amost certainly lead to bugs because people will
  -- forget this. Maybe we can flip the internal encoding to be
  -- non-null by default and needing explicit null-encoding (via
  -- Maybe).
  getAnnotatedType :: AnnotatedType GraphQL.Schema.Type

instance forall a. HasAnnotatedType a => HasAnnotatedType (Maybe a) where
  -- see TODO in HasAnnotatedType class
  getAnnotatedType =
    let TypeNonNull (NonNullTypeNamed t) = getAnnotatedType @a
    in TypeNamed t

instance HasAnnotatedType Int where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GInt

instance HasAnnotatedType Bool where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GBool

instance HasAnnotatedType Text where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GString

instance HasAnnotatedType Double where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GFloat

instance HasAnnotatedType Float where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GFloat

instance forall t. (HasAnnotatedType t) => HasAnnotatedType (List t) where
  getAnnotatedType = TypeList (ListType (getAnnotatedType @t))

instance forall ks enum. (KnownSymbol ks, GraphQLEnum enum) => HasAnnotatedType (Enum ks enum) where
  getAnnotatedType =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        et = EnumTypeDefinition name (map (EnumValueDefinition . Name . toS) (enumValues @enum))
    in TypeNonNull (NonNullTypeNamed (DefinedType (TypeDefinitionEnum et)))

instance forall ks as. (KnownSymbol ks, UnionTypeObjectTypeDefinitionList as) => HasAnnotatedType (Union ks as) where
  getAnnotatedType =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        types = NonEmptyList (getUnionTypeObjectTypeDefinitions @as)
    in TypeNamed (DefinedType (TypeDefinitionUnion (UnionTypeDefinition name types)))

-- Help users with better type errors
instance GHC.TypeLits.TypeError ('GHC.TypeLits.Text "Cannot encode Integer because it has arbitrary size but the JSON encoding is a number") =>
         HasAnnotatedType Integer where
  getAnnotatedType = undefined


-- Builtin input types
class HasAnnotatedInputType a where
  -- See TODO comment in "HasAnnotatedType" class for nullability.
  getAnnotatedInputType :: AnnotatedType InputType

instance forall a. HasAnnotatedInputType a => HasAnnotatedInputType (Maybe a) where
  getAnnotatedInputType =
    let TypeNonNull (NonNullTypeNamed t) = getAnnotatedInputType @a
    in TypeNamed t

instance HasAnnotatedInputType Int where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GInt

instance HasAnnotatedInputType Bool where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GBool

instance HasAnnotatedInputType Text where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GString

instance HasAnnotatedInputType Double where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GFloat

instance HasAnnotatedInputType Float where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GFloat

instance forall t. (HasAnnotatedInputType t) => HasAnnotatedInputType (List t) where
  getAnnotatedInputType = TypeList (ListType (getAnnotatedInputType @t))

instance forall ks enum. (KnownSymbol ks, GraphQLEnum enum) => HasAnnotatedInputType (Enum ks enum) where
  getAnnotatedInputType =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        et = EnumTypeDefinition name (map (EnumValueDefinition . Name . toS) (enumValues @enum))
    in TypeNonNull (NonNullTypeNamed (DefinedInputType (InputTypeDefinitionEnum et)))
