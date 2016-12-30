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
  -- * Getting names
  , HasName(..)
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

import GraphQL.Value (Value)
import GraphQL.Internal.AST (Name, unsafeMakeName)

-- | Types that implement this have values with canonical names in GraphQL.
--
-- e.g. a field @foo(bar: Int32)@ would have the name @\"foo\"@.
--
-- If a thing *might* have a name, or has a name that might not be valid,
-- don't use this.
--
-- If a thing is aliased, then return the *original* name.
class HasName a where
  -- | Get the name of the object.
  getName :: a -> Name

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

instance HasName Type where
  getName (DefinedType x) = getName x
  getName (BuiltinType x) = getName x

data TypeDefinition = TypeDefinitionObject        ObjectTypeDefinition
                    | TypeDefinitionInterface     InterfaceTypeDefinition
                    | TypeDefinitionUnion         UnionTypeDefinition
                    | TypeDefinitionScalar        ScalarTypeDefinition
                    | TypeDefinitionEnum          EnumTypeDefinition
                    | TypeDefinitionInputObject   InputObjectTypeDefinition
                    | TypeDefinitionTypeExtension TypeExtensionDefinition
                      deriving (Eq, Show)

instance HasName TypeDefinition where
  getName (TypeDefinitionObject x) = getName x
  getName (TypeDefinitionInterface x) = getName x
  getName (TypeDefinitionUnion x) = getName x
  getName (TypeDefinitionScalar x) = getName x
  getName (TypeDefinitionEnum x) = getName x
  getName (TypeDefinitionInputObject x) = getName x
  getName (TypeDefinitionTypeExtension x) = getName x

data ObjectTypeDefinition = ObjectTypeDefinition Name Interfaces (NonEmptyList FieldDefinition)
                            deriving (Eq, Show)

instance HasName ObjectTypeDefinition where
  getName (ObjectTypeDefinition name _ _) = name

type Interfaces = [InterfaceTypeDefinition]

data FieldDefinition = FieldDefinition Name [ArgumentDefinition] (AnnotatedType Type)
                       deriving (Eq, Show)

instance HasName FieldDefinition where
  getName (FieldDefinition name _ _) = name

data ArgumentDefinition = ArgumentDefinition Name (AnnotatedType InputType) (Maybe DefaultValue)
                          deriving (Eq, Show)

instance HasName ArgumentDefinition where
  getName (ArgumentDefinition name _ _) = name

data InterfaceTypeDefinition = InterfaceTypeDefinition Name (NonEmptyList FieldDefinition)
                               deriving (Eq, Show)

instance HasName InterfaceTypeDefinition where
  getName (InterfaceTypeDefinition name _) = name

data UnionTypeDefinition = UnionTypeDefinition Name (NonEmptyList ObjectTypeDefinition)
                           deriving (Eq, Show)

instance HasName UnionTypeDefinition where
  getName (UnionTypeDefinition name _) = name

newtype ScalarTypeDefinition = ScalarTypeDefinition Name
                             deriving (Eq, Show)

instance HasName ScalarTypeDefinition where
  getName (ScalarTypeDefinition name) = name

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

instance HasName Builtin where
  getName = unsafeMakeName . getBuiltinName
    where
      getBuiltinName GInt = "Int"
      getBuiltinName GBool = "Boolean"
      getBuiltinName GString = "String"
      getBuiltinName GFloat = "Float"
      getBuiltinName GID = "ID"

data EnumTypeDefinition = EnumTypeDefinition Name [EnumValueDefinition]
                          deriving (Eq, Show)

instance HasName EnumTypeDefinition where
  getName (EnumTypeDefinition name _) = name

newtype EnumValueDefinition = EnumValueDefinition Name
                              deriving (Eq, Show)

instance HasName EnumValueDefinition where
  getName (EnumValueDefinition name) = name

data InputObjectTypeDefinition = InputObjectTypeDefinition Name (NonEmptyList InputObjectFieldDefinition)
                                 deriving (Eq, Show)

instance HasName InputObjectTypeDefinition where
  getName (InputObjectTypeDefinition name _) = name

data InputObjectFieldDefinition = InputObjectFieldDefinition Name (AnnotatedType InputType) (Maybe DefaultValue)
                                  deriving (Eq, Show) -- XXX: spec is unclear about default value for input object field definitions

instance HasName InputObjectFieldDefinition where
  getName (InputObjectFieldDefinition name _ _) = name

newtype TypeExtensionDefinition = TypeExtensionDefinition ObjectTypeDefinition
                                  deriving (Eq, Show)

instance HasName TypeExtensionDefinition where
  getName (TypeExtensionDefinition obj) = getName obj

data InputType = DefinedInputType InputTypeDefinition | BuiltinInputType Builtin deriving (Eq, Show)

instance HasName InputType where
  getName (DefinedInputType x) = getName x
  getName (BuiltinInputType x) = getName x

data InputTypeDefinition
  = InputTypeDefinitionObject        InputObjectTypeDefinition
  | InputTypeDefinitionScalar        ScalarTypeDefinition
  | InputTypeDefinitionEnum          EnumTypeDefinition
  deriving (Eq, Show)

instance HasName InputTypeDefinition where
  getName (InputTypeDefinitionObject x) = getName x
  getName (InputTypeDefinitionScalar x) = getName x
  getName (InputTypeDefinitionEnum x) = getName x

-- | A literal value specified as a default as part of a type definition.
--
-- Use this type alias when you want to be clear that a definition may include
-- some sort of default value.
--
-- Arguments (see 'ArgumentDefinition') and fields within input objects (see
-- 'InputObjectFieldDefinition') can have default values. These are allowed to
-- be any kind of literal.
type DefaultValue = Value
