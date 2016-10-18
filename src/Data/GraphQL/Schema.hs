-- | Sketch of fully-realized schema type system.
--
-- Differs from
-- http://hackage.haskell.org/package/graphql-0.3/docs/Data-GraphQL-AST.html#g:6
-- in that there are no type references.
--
-- Intended as a first step on the path to a type-level GraphQL schema definition.

module Data.GraphQL.Schema where

import Protolude hiding (Type)

import Data.Text (Text)


newtype Name = Name Text deriving (Eq, Show) -- XXX: Phantom type?

newtype NonEmptyList a = NonEmptyList [a] deriving (Eq, Show)

data AnnotatedType t = TypeNamed t
                     | TypeList (ListType t)
                     | TypeNonNull (NonNullType t)
                     deriving (Eq,Show)

data ListType t = ListType (AnnotatedType t) deriving (Eq,Show)

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

data ScalarTypeDefinition = ScalarTypeDefinition Name
                            deriving (Eq, Show)

data Builtin = GInt | GBool | GString | GFloat | GID deriving (Eq, Show)

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

data InputTypeDefinition = InputTypeDefinitionObject        ObjectTypeDefinition
                         | InputTypeDefinitionScalar        ScalarTypeDefinition
                         | InputTypeDefinitionEnum          EnumTypeDefinition
                         | InputTypeDefinitionInputObject   InputObjectTypeDefinition
                      deriving (Eq, Show)


type DefaultValue = InputValue

data InputValue
  = ValueInt Int
  | ValueFloat Double
  | ValueBoolean Bool
  | ValueString Text
  | ValueEnum Name
  | ValueList ListValue
  | ValueObject [ObjectField] deriving (Eq, Show)

data ListValue = ListValue [InputValue] deriving (Eq, Show)
data ObjectValue = ObjectValue [ObjectField] deriving (Eq, Show)
data ObjectField = ObjectField Name InputValue deriving (Eq, Show)
