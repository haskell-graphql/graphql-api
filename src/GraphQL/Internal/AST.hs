{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module GraphQL.Internal.AST
  ( Name(getNameText)
  , nameParser
  , makeName
  , unsafeMakeName
  , unsafeNameFromSymbol
  , Document(..)
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
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Test.QuickCheck (Arbitrary(..), elements, listOf)

import GraphQL.Internal.Tokens (tok)

-- * Name

-- | A name in GraphQL.
--
-- https://facebook.github.io/graphql/#sec-Names
newtype Name = Name { getNameText :: Text } deriving (Eq, Ord, Show)

instance Aeson.ToJSON Name where
  toJSON = Aeson.toJSON . getNameText

instance Arbitrary Name where
  arbitrary = do
    initial <- elements alpha
    rest <- listOf (elements (alpha <> numeric))
    pure (unsafeMakeName (toS (initial:rest)))
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

-- | Create a 'Name'.
--
-- Names must match the regex @[_A-Za-z][_0-9A-Za-z]*@. If the given text does
-- not match, return Nothing.
--
-- >>> makeName "foo"
-- Just (Name {getNameText = "foo"})
-- >>> makeName "9-bar"
-- Nothing
makeName :: Text -> Maybe Name
makeName = hush . A.parseOnly nameParser

-- | Create a 'Name', panicking if the given text is invalid.
--
-- Prefer 'makeName' to this in all cases.
--
-- >>> unsafeMakeName "foo"
-- Name {getNameText = "foo"}
unsafeMakeName :: Text -> Name
unsafeMakeName name = fromMaybe (panic $ "Not a valid GraphQL name: " <> show name) (makeName name)

-- | Convert a type-level 'Symbol' into a GraphQL 'Name'.
--
-- Panics if the name is not valid GraphQL.
unsafeNameFromSymbol :: forall (n :: Symbol) (proxy :: Symbol -> *). KnownSymbol n => proxy n -> Name
unsafeNameFromSymbol = unsafeMakeName . toS . symbolVal


-- * Document

newtype Document = Document { getDefinitions :: [Definition] } deriving (Eq,Show)

data Definition = DefinitionOperation OperationDefinition
                | DefinitionFragment  FragmentDefinition
                | DefinitionType      TypeDefinition
                  deriving (Eq,Show)

data OperationDefinition = Query    { getNode :: Node }
                         | Mutation { getNode :: Node }
                           deriving (Eq,Show)

data Node = Node (Maybe Name) [VariableDefinition] [Directive] SelectionSet
            deriving (Eq,Show)

-- XXX: Lots of things have names. Maybe we should define a typeclass for
-- getting the name?
getNodeName :: Node -> Maybe Name
getNodeName (Node name _ _ _) = name

data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)
                          deriving (Eq,Show)

newtype Variable = Variable Name deriving (Eq,Show)

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
