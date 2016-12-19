{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Fully realized GraphQL schema type system at the value level.
--
-- Differs from "Data.GraphQL.AST" in the
-- [graphql](http://hackage.haskell.org/package/graphql) package in that there
-- are no type references. Instead, everything is inlined.
--
-- Equivalent representation of GraphQL /values/ is in "GraphQL.Value".
module GraphQL.Internal.Schema
  ( Type(..)
  -- * Builtin types
  , Builtin(..)
  -- * Defining new types
  , TypeDefinition(..)
  , Name
  , ArgumentDefinition(..)
  , EnumValueDefinition(..)
  , EnumTypeDefinition(..)
  , FieldDefinition(..)
  , Interfaces
  , InterfaceTypeDefinition(..)
  , NonEmptyList(..)
  , ObjectTypeDefinition(..)
  , UnionTypeDefinition(..)
  -- ** Input types
  , InputType(..)
  , InputTypeDefinition(..)
  -- * Using existing types
  , AnnotatedType(..)
  , ListType(..)
  , NonNullType(..)
  ) where

import Protolude hiding (Type)

import GraphQL.Value (Name, Value)

-- XXX: Use the built-in NonEmptyList in Haskell
newtype NonEmptyList a = NonEmptyList [a] deriving (Eq, Show)

data AnnotatedType t = TypeNamed t
                     | TypeList (ListType t)
                     | TypeNonNull (NonNullType t)
                     deriving (Eq,Show)

newtype ListType t = ListType (AnnotatedType t) deriving (Eq, Show)

data NonNullType t = NonNullTypeNamed t
                   | NonNullTypeList  (ListType t)
                   deriving (Eq,Show)

data Type = DefinedType TypeDefinition | BuiltinType Builtin deriving (Eq, Show)

data TypeDefinition = TypeDefinitionObject        ObjectTypeDefinition
                    | TypeDefinitionInterface     InterfaceTypeDefinition
                    | TypeDefinitionUnion         UnionTypeDefinition
                    | TypeDefinitionScalar        ScalarTypeDefinition
                    | TypeDefinitionEnum          EnumTypeDefinition
                    | TypeDefinitionInputObject   InputObjectTypeDefinition
                    | TypeDefinitionTypeExtension TypeExtensionDefinition
                      deriving (Eq, Show)

data ObjectTypeDefinition = ObjectTypeDefinition Name Interfaces (NonEmptyList FieldDefinition)
                            deriving (Eq, Show)

type Interfaces = [InterfaceTypeDefinition]

data FieldDefinition = FieldDefinition Name [ArgumentDefinition] (AnnotatedType Type)
                       deriving (Eq, Show)

data ArgumentDefinition = ArgumentDefinition Name (AnnotatedType InputType) (Maybe DefaultValue)
                          deriving (Eq, Show)

data InterfaceTypeDefinition = InterfaceTypeDefinition Name (NonEmptyList FieldDefinition)
                               deriving (Eq, Show)

data UnionTypeDefinition = UnionTypeDefinition Name (NonEmptyList ObjectTypeDefinition)
                           deriving (Eq, Show)

newtype ScalarTypeDefinition = ScalarTypeDefinition Name
                             deriving (Eq, Show)

-- | Types that are built into GraphQL.
--
-- The GraphQL spec refers to these as
-- \"[scalars](https://facebook.github.io/graphql/#sec-Scalars)\".
data Builtin
  -- | A signed 32‐bit numeric non‐fractional value
  = GInt
  -- | True or false
  | GBool
  -- | Textual data represented as UTF-8 character sequences
  | GString
  -- | Signed double‐precision fractional values as specified by [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point)
  | GFloat
  -- | A unique identifier, often used to refetch an object or as the key for a cache
  | GID deriving (Eq, Show)

data EnumTypeDefinition = EnumTypeDefinition Name [EnumValueDefinition]
                          deriving (Eq, Show)

newtype EnumValueDefinition = EnumValueDefinition Name
                              deriving (Eq, Show)

data InputObjectTypeDefinition = InputObjectTypeDefinition Name (NonEmptyList InputObjectFieldDefinition)
                                 deriving (Eq, Show)

data InputObjectFieldDefinition = InputObjectFieldDefinition Name (AnnotatedType InputType) (Maybe DefaultValue)
                                  deriving (Eq, Show) -- XXX: spec is unclear about default value for input object field definitions

newtype TypeExtensionDefinition = TypeExtensionDefinition ObjectTypeDefinition
                                  deriving (Eq, Show)


data InputType = DefinedInputType InputTypeDefinition | BuiltinInputType Builtin deriving (Eq, Show)


data InputTypeDefinition
  = InputTypeDefinitionObject        InputObjectTypeDefinition
  | InputTypeDefinitionScalar        ScalarTypeDefinition
  | InputTypeDefinitionEnum          EnumTypeDefinition
  deriving (Eq, Show)


-- | A literal value specified as a default as part of a type definition.
--
-- Use this type alias when you want to be clear that a definition may include
-- some sort of default value.
--
-- Arguments (see 'ArgumentDefinition') and fields within input objects (see
-- 'InputObjectFieldDefinition') can have default values. These are allowed to
-- be any kind of literal.
type DefaultValue = Value
