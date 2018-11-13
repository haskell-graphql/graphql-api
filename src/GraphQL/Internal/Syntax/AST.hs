{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Description: The GraphQL AST
module GraphQL.Internal.Syntax.AST
  ( QueryDocument(..)
  , SchemaDocument(..)
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
  , Position(..)
  , StringValue(..)
  , ListValue(..)
  , ObjectValue(..)
  , ObjectField(..)
  , DefaultValue
  , Directive(..)
  , GType(..)
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

import Protolude

import Test.QuickCheck (Arbitrary(..), listOf, oneof)
import GraphQL.Internal.Arbitrary (arbitraryText)
import GraphQL.Internal.Name
  ( Name
  , HasName(..)
  )

-- * Documents

-- | A 'QueryDocument' is something a user might send us.
--
-- https://facebook.github.io/graphql/#sec-Language.Query-Document
data QueryDocument = QueryDocument {
  getDefinitions :: [Definition]
  , position :: PositionInfo
  } deriving (Eq,Show)

-- | PositionInfo for a start position and an end position.
--
-- When the AST is constructed elsewhere(not by parsing), for encoding as an example,
-- the PositionInfo shall be Nothing
type PositionInfo = Maybe (Int, Int)
data Definition = DefinitionOperation OperationDefinition PositionInfo
                | DefinitionFragment  FragmentDefinition PositionInfo
                deriving (Eq,Show)

-- | A 'SchemaDocument' is a document that defines a GraphQL schema.
--
-- https://facebook.github.io/graphql/#sec-Type-System
newtype SchemaDocument = SchemaDocument [TypeDefinition] deriving (Eq, Show)

data OperationDefinition
  = Query Node PositionInfo
  | Mutation Node PositionInfo
  | AnonymousQuery SelectionSet PositionInfo
  deriving (Eq,Show)

data Node = Node (Maybe Name) [VariableDefinition] [Directive] SelectionSet PositionInfo
            deriving (Eq,Show)

data VariableDefinition = VariableDefinition Variable GType (Maybe DefaultValue) PositionInfo
                          deriving (Eq,Show)

newtype Variable = Variable Name deriving (Eq, Ord, Show)

instance Arbitrary Variable where
  arbitrary = Variable <$> arbitrary

type SelectionSet = [Selection]

data Selection = SelectionField Field PositionInfo
               | SelectionFragmentSpread FragmentSpread PositionInfo
               | SelectionInlineFragment InlineFragment PositionInfo
                 deriving (Eq,Show)

data Field = Field (Maybe Alias) Name [Argument] [Directive] SelectionSet
             deriving (Eq,Show)

type Alias = Name

data Argument = Argument Name Value deriving (Eq,Show)

-- * Fragments

data FragmentSpread = FragmentSpread Name [Directive]
                      deriving (Eq,Show)

data InlineFragment =
  InlineFragment (Maybe TypeCondition) [Directive] SelectionSet
  deriving (Eq,Show)

data FragmentDefinition =
  FragmentDefinition Name TypeCondition [Directive] SelectionSet
  deriving (Eq,Show)

type TypeCondition = NamedType

-- * Values

data Value  = ValueVariable Variable PositionInfo
           | ValueInt Int32 PositionInfo
           -- GraphQL Float is double precison
           | ValueFloat Double PositionInfo
           | ValueBoolean Bool PositionInfo
           | ValueString StringValue PositionInfo
           | ValueEnum Name PositionInfo
           | ValueList ListValue PositionInfo
           | ValueObject ObjectValue PositionInfo
           | ValueNull PositionInfo
           deriving (Eq, Show)

class Position a where
  setPos :: (PositionInfo -> PositionInfo) -> a -> a

instance Position Value where
  setPos f (ValueNull p) = ValueNull (f p)
  setPos f (ValueVariable x p) = ValueVariable x (f p)
  setPos f (ValueInt x p) = ValueInt x (f p)
  setPos f (ValueFloat x p) = ValueFloat x (f p)
  setPos f (ValueBoolean x p) = ValueBoolean x (f p)
  setPos f (ValueString x p) = ValueString x (f p)
  setPos f (ValueEnum x p) = ValueEnum x (f p)
  setPos f (ValueList x p) = ValueList (setPos f x) (f p)
  setPos f (ValueObject x p) = ValueObject (setPos f x) (f p)

-- TODO: Use Rep
instance Position ListValue where
  setPos f (ListValue x) = ListValue (map (setPos f) x)
instance Position ObjectValue where
  setPos f (ObjectValue x) = ObjectValue (map (setPos f) x)
instance Position ObjectField where
  setPos f (ObjectField x v) = ObjectField x (setPos f v)
  
instance Arbitrary Value where
  arbitrary = oneof $ map (<*> pure Nothing) [ ValueVariable <$> arbitrary
                    , ValueInt <$> arbitrary
                    , ValueFloat <$> arbitrary
                    , ValueBoolean <$> arbitrary
                    , ValueString <$> arbitrary
                    , ValueEnum <$> arbitrary
                    , ValueList <$> arbitrary
                    , ValueObject <$> arbitrary
                    , pure ValueNull
                    ]

newtype StringValue = StringValue Text deriving (Eq,Show)

instance Arbitrary StringValue where
  arbitrary = StringValue <$> arbitraryText

newtype ListValue = ListValue [Value] deriving (Eq,Show)

instance Arbitrary ListValue where
  arbitrary = ListValue <$> listOf arbitrary

newtype ObjectValue = ObjectValue [ObjectField] deriving (Eq,Show)

instance Arbitrary ObjectValue where
  arbitrary = ObjectValue <$> listOf arbitrary

data ObjectField = ObjectField Name Value deriving (Eq,Show)

instance Arbitrary ObjectField where
  arbitrary = ObjectField <$> arbitrary <*> arbitrary

type DefaultValue = Value

-- * Directives

data Directive = Directive Name [Argument] deriving (Eq,Show)

-- * Type Reference

data GType = TypeNamed NamedType
           | TypeList ListType
           | TypeNonNull NonNullType
           deriving (Eq, Ord, Show)

-- | Get the name of the given 'GType'.
instance HasName GType where
  getName (TypeNamed (NamedType n)) = n
  getName (TypeList (ListType t)) = getName t
  getName (TypeNonNull (NonNullTypeNamed (NamedType n))) = n
  getName (TypeNonNull (NonNullTypeList (ListType l))) = getName l

newtype NamedType = NamedType Name deriving (Eq, Ord, Show)

newtype ListType = ListType GType deriving (Eq, Ord, Show)

data NonNullType = NonNullTypeNamed NamedType
                 | NonNullTypeList  ListType
                   deriving (Eq, Ord, Show)

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

data FieldDefinition = FieldDefinition Name ArgumentsDefinition GType
                       deriving (Eq,Show)

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition = InputValueDefinition Name GType (Maybe DefaultValue)
                            deriving (Eq,Show)

data InterfaceTypeDefinition = InterfaceTypeDefinition Name [FieldDefinition]
                               deriving (Eq,Show)

data UnionTypeDefinition = UnionTypeDefinition Name [NamedType]
                           deriving (Eq,Show)

newtype ScalarTypeDefinition = ScalarTypeDefinition Name
                             deriving (Eq,Show)

data EnumTypeDefinition = EnumTypeDefinition Name [EnumValueDefinition]
                          deriving (Eq,Show)

newtype EnumValueDefinition = EnumValueDefinition Name
                              deriving (Eq,Show)

data InputObjectTypeDefinition = InputObjectTypeDefinition Name [InputValueDefinition]
                                 deriving (Eq,Show)

newtype TypeExtensionDefinition = TypeExtensionDefinition ObjectTypeDefinition
                                  deriving (Eq,Show)
