module GraphQL.Internal.AST
  ( Name
  , Document(..)
  , Definition(..)
  , OperationDefinition(..)
  , Node(..)
  , VariableDefinition(..)
  , Variable(..)
  , SelectionSet
  , Selection(..)
  , Field(..)
  , Alias
  , Argument(..)
  , FragmentSpread(..)
  , InlineFragment(..)
  , FragmentDefinition(..)
  , TypeCondition
  , Value(..)
  , StringValue(..)
  , ListValue(..)
  , ObjectValue(..)
  , ObjectField(..)
  , DefaultValue
  , Directive(..)
  , Type(..)
  , NamedType(..)
  , ListType(..)
  , NonNullType(..)
  , TypeDefinition(..)
  , ObjectTypeDefinition(..)
  , Interfaces
  , FieldDefinition(..)
  , ArgumentsDefinition
  , InputValueDefinition(..)
  , InterfaceTypeDefinition(..)
  , UnionTypeDefinition(..)
  , ScalarTypeDefinition(..)
  , EnumTypeDefinition(..)
  , EnumValueDefinition(..)
  , InputObjectTypeDefinition(..)
  , TypeExtensionDefinition(..)
  ) where

import Protolude hiding (Type)

import Data.Int (Int32)
import Data.Text (Text)

-- * Name

type Name = Text

-- * Document

newtype Document = Document [Definition] deriving (Eq,Show)

data Definition = DefinitionOperation OperationDefinition
                | DefinitionFragment  FragmentDefinition
                | DefinitionType      TypeDefinition
                  deriving (Eq,Show)

data OperationDefinition = Query    Node
                         | Mutation Node
                           deriving (Eq,Show)

data Node = Node Name [VariableDefinition] [Directive] SelectionSet
            deriving (Eq,Show)

data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)
                          deriving (Eq,Show)

newtype Variable = Variable Name deriving (Eq,Show)

type SelectionSet = [Selection]

data Selection = SelectionField Field
               | SelectionFragmentSpread FragmentSpread
               | SelectionInlineFragment InlineFragment
                 deriving (Eq,Show)

data Field = Field Alias Name [Argument]
                              [Directive]
                              SelectionSet
             deriving (Eq,Show)

type Alias = Name

data Argument = Argument Name Value deriving (Eq,Show)

-- * Fragments

data FragmentSpread = FragmentSpread Name [Directive]
                      deriving (Eq,Show)

data InlineFragment =
  InlineFragment TypeCondition [Directive] SelectionSet
  deriving (Eq,Show)

data FragmentDefinition =
  FragmentDefinition Name TypeCondition [Directive] SelectionSet
  deriving (Eq,Show)

type TypeCondition = NamedType

-- * Values

data Value = ValueVariable Variable
           | ValueInt Int32
           -- GraphQL Float is double precison
           | ValueFloat Double
           | ValueBoolean Bool
           | ValueString StringValue
           | ValueEnum Name
           | ValueList ListValue
           | ValueObject ObjectValue
             deriving (Eq,Show)

newtype StringValue = StringValue Text deriving (Eq,Show)

newtype ListValue = ListValue [Value] deriving (Eq,Show)

newtype ObjectValue = ObjectValue [ObjectField] deriving (Eq,Show)

data ObjectField = ObjectField Name Value deriving (Eq,Show)

type DefaultValue = Value

-- * Directives

data Directive = Directive Name [Argument] deriving (Eq,Show)

-- * Type Reference

data Type = TypeNamed NamedType
          | TypeList ListType
          | TypeNonNull NonNullType
            deriving (Eq,Show)

newtype NamedType = NamedType Name deriving (Eq,Show)

newtype ListType = ListType Type deriving (Eq,Show)

data NonNullType = NonNullTypeNamed NamedType
                 | NonNullTypeList  ListType
                   deriving (Eq,Show)

-- * Type definition

data TypeDefinition = TypeDefinitionObject        ObjectTypeDefinition
                    | TypeDefinitionInterface     InterfaceTypeDefinition
                    | TypeDefinitionUnion         UnionTypeDefinition
                    | TypeDefinitionScalar        ScalarTypeDefinition
                    | TypeDefinitionEnum          EnumTypeDefinition
                    | TypeDefinitionInputObject   InputObjectTypeDefinition
                    | TypeDefinitionTypeExtension TypeExtensionDefinition
                      deriving (Eq,Show)

data ObjectTypeDefinition = ObjectTypeDefinition Name Interfaces [FieldDefinition]
                            deriving (Eq,Show)

type Interfaces = [NamedType]

data FieldDefinition = FieldDefinition Name ArgumentsDefinition Type
                       deriving (Eq,Show)

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition = InputValueDefinition Name Type (Maybe DefaultValue)
                            deriving (Eq,Show)

data InterfaceTypeDefinition = InterfaceTypeDefinition Name [FieldDefinition]
                               deriving (Eq,Show)

data UnionTypeDefinition = UnionTypeDefinition Name [NamedType]
                           deriving (Eq,Show)

data ScalarTypeDefinition = ScalarTypeDefinition Name
                            deriving (Eq,Show)

data EnumTypeDefinition = EnumTypeDefinition Name [EnumValueDefinition]
                          deriving (Eq,Show)

newtype EnumValueDefinition = EnumValueDefinition Name
                              deriving (Eq,Show)

data InputObjectTypeDefinition = InputObjectTypeDefinition Name [InputValueDefinition]
                                 deriving (Eq,Show)

newtype TypeExtensionDefinition = TypeExtensionDefinition ObjectTypeDefinition
                                  deriving (Eq,Show)
