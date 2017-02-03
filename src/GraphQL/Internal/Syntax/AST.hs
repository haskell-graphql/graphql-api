{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GraphQL.Internal.Syntax.AST
  ( Name(unName)
  , nameParser
  , NameError(..)
  , unsafeMakeName
  , makeName
  , QueryDocument(..)
  , SchemaDocument(..)
  , Definition(..)
  , OperationDefinition(..)
  , Node(..)
  , getNodeName
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

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as A
import Data.Char (isDigit)
import Data.String (IsString(..))
import Test.QuickCheck (Arbitrary(..), elements, listOf, oneof)

import GraphQL.Internal.Arbitrary (arbitraryText)
import GraphQL.Internal.Syntax.Tokens (tok)

-- * Name

-- | A name in GraphQL.
--
-- https://facebook.github.io/graphql/#sec-Names
newtype Name = Name { unName :: Text } deriving (Eq, Ord, Show)

-- | Create a 'Name', panicking if the given text is invalid.
--
-- Prefer 'makeName' to this in all cases.
--
-- >>> unsafeMakeName "foo"
-- Name {unName = "foo"}
unsafeMakeName :: HasCallStack => Text -> Name
unsafeMakeName name =
  case makeName name of
    Left e -> panic (show e)
    Right n -> n

-- | Create a 'Name'.
--
-- Names must match the regex @[_A-Za-z][_0-9A-Za-z]*@. If the given text does
-- not match, return Nothing.
--
-- >>> makeName "foo"
-- Right (Name {unName = "foo"})
-- >>> makeName "9-bar"
-- Left (NameError "9-bar")
makeName :: Text -> Either NameError Name
makeName name = first (const (NameError name)) (A.parseOnly nameParser name)

-- | An invalid name.
newtype NameError = NameError Text deriving (Eq, Show)


instance IsString Name where
  fromString = unsafeMakeName . toS

instance Aeson.ToJSON Name where
  toJSON = Aeson.toJSON . unName

instance Arbitrary Name where
  arbitrary = do
    initial <- elements alpha
    rest <- listOf (elements (alpha <> numeric))
    pure (Name (toS (initial:rest)))
    where
      alpha = ['A'..'Z'] <> ['a'..'z'] <> ['_']
      numeric = ['0'..'9']

-- | Parser for 'Name'.
nameParser :: A.Parser Name
nameParser = Name <$> tok ((<>) <$> A.takeWhile1 isA_z
                                <*> A.takeWhile ((||) <$> isDigit <*> isA_z))
  where
    -- `isAlpha` handles many more Unicode Chars
    isA_z = A.inClass $ '_' : ['A'..'Z'] <> ['a'..'z']

-- * Documents

-- | A 'QueryDocument' is something a user might send us.
--
-- https://facebook.github.io/graphql/#sec-Language.Query-Document
newtype QueryDocument = QueryDocument { getDefinitions :: [Definition] } deriving (Eq,Show)

data Definition = DefinitionOperation OperationDefinition
                | DefinitionFragment  FragmentDefinition
                deriving (Eq,Show)

-- | A 'SchemaDocument' is a document that defines a GraphQL schema.
--
-- https://facebook.github.io/graphql/#sec-Type-System
newtype SchemaDocument = SchemaDocument [TypeDefinition] deriving (Eq, Show)

data OperationDefinition
  = Query Node
  | Mutation Node
  | AnonymousQuery SelectionSet
  deriving (Eq,Show)

data Node = Node Name [VariableDefinition] [Directive] SelectionSet
            deriving (Eq,Show)

-- TODO: Just make Node implement HasName.
getNodeName :: Node -> Name
getNodeName (Node name _ _ _) = name

data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)
                          deriving (Eq,Show)

newtype Variable = Variable Name deriving (Eq, Ord, Show)

instance Arbitrary Variable where
  arbitrary = Variable <$> arbitrary

type SelectionSet = [Selection]

data Selection = SelectionField Field
               | SelectionFragmentSpread FragmentSpread
               | SelectionInlineFragment InlineFragment
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

data Value = ValueVariable Variable
           | ValueInt Int32
           -- GraphQL Float is double precison
           | ValueFloat Double
           | ValueBoolean Bool
           | ValueString StringValue
           | ValueEnum Name
           | ValueList ListValue
           | ValueObject ObjectValue
           | ValueNull
           deriving (Eq, Show)

instance Arbitrary Value where
  arbitrary = oneof [ ValueVariable <$> arbitrary
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

data Type = TypeNamed NamedType
          | TypeList ListType
          | TypeNonNull NonNullType
            deriving (Eq, Ord, Show)

newtype NamedType = NamedType Name deriving (Eq, Ord, Show)

newtype ListType = ListType Type deriving (Eq, Ord, Show)

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

data FieldDefinition = FieldDefinition Name ArgumentsDefinition Type
                       deriving (Eq,Show)

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition = InputValueDefinition Name Type (Maybe DefaultValue)
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
