module GraphQL.Internal.Parser
  ( document
  , name
  ) where

import Protolude hiding (Type, takeWhile)

import Control.Applicative ((<|>), empty, many, optional)
import Control.Monad (when)
import Data.Char (isDigit, isSpace)
import Data.Foldable (traverse_)

import Data.Text (Text, append)
import Data.Attoparsec.Text
  ( Parser
  , (<?>)
  , anyChar
  , decimal
  , double
  , endOfLine
  , inClass
  , many1
  , manyTill
  , option
  , peekChar
  , sepBy1
  , signed
  , takeWhile
  , takeWhile1
  )

import qualified GraphQL.Internal.AST as AST

-- * Name

name :: Parser AST.Name
name = tok $ append <$> takeWhile1 isA_z
                    <*> takeWhile ((||) <$> isDigit <*> isA_z)
  where
    -- `isAlpha` handles many more Unicode Chars
    isA_z =  inClass $ '_' : ['A'..'Z'] ++ ['a'..'z']

-- * Document

document :: Parser AST.Document
document = whiteSpace
   *> (AST.Document <$> many1 definition)
  -- Try SelectionSet when no definition
  <|> (AST.Document . pure
        . AST.DefinitionOperation
        . AST.Query
        . AST.Node mempty empty empty
        <$> selectionSet)
  <?> "document error!"

definition :: Parser AST.Definition
definition = AST.DefinitionOperation <$> operationDefinition
         <|> AST.DefinitionFragment  <$> fragmentDefinition
         <|> AST.DefinitionType      <$> typeDefinition
         <?> "definition error!"

operationDefinition :: Parser AST.OperationDefinition
operationDefinition =
      AST.Query    <$ tok "query"    <*> node
  <|> AST.Mutation <$ tok "mutation" <*> node
  <?> "operationDefinition error!"

node :: Parser AST.Node
node = AST.Node <$> name
                <*> optempty variableDefinitions
                <*> optempty directives
                <*> selectionSet

variableDefinitions :: Parser [AST.VariableDefinition]
variableDefinitions = parens (many1 variableDefinition)

variableDefinition :: Parser AST.VariableDefinition
variableDefinition =
  AST.VariableDefinition <$> variable
                         <*  tok ":"
                         <*> type_
                         <*> optional defaultValue

defaultValue :: Parser AST.DefaultValue
defaultValue = tok "=" *> value

variable :: Parser AST.Variable
variable = AST.Variable <$ tok "$" <*> name

selectionSet :: Parser AST.SelectionSet
selectionSet = braces $ many1 selection

selection :: Parser AST.Selection
selection = AST.SelectionField <$> field
            -- Inline first to catch `on` case
        <|> AST.SelectionInlineFragment <$> inlineFragment
        <|> AST.SelectionFragmentSpread <$> fragmentSpread
        <?> "selection error!"

field :: Parser AST.Field
field = AST.Field <$> optempty alias
                  <*> name
                  <*> optempty arguments
                  <*> optempty directives
                  <*> optempty selectionSet

alias :: Parser AST.Alias
alias = name <* tok ":"

arguments :: Parser [AST.Argument]
arguments = parens $ many1 argument

argument :: Parser AST.Argument
argument = AST.Argument <$> name <* tok ":" <*> value

-- * Fragments

fragmentSpread :: Parser AST.FragmentSpread
-- TODO: Make sure it fails when `... on`.
-- See https://facebook.github.io/graphql/#FragmentSpread
fragmentSpread = AST.FragmentSpread
  <$  tok "..."
  <*> name
  <*> optempty directives

-- InlineFragment tried first in order to guard against 'on' keyword
inlineFragment :: Parser AST.InlineFragment
inlineFragment = AST.InlineFragment
  <$  tok "..."
  <*  tok "on"
  <*> typeCondition
  <*> optempty directives
  <*> selectionSet

fragmentDefinition :: Parser AST.FragmentDefinition
fragmentDefinition = AST.FragmentDefinition
  <$  tok "fragment"
  <*> name
  <*  tok "on"
  <*> typeCondition
  <*> optempty directives
  <*> selectionSet

typeCondition :: Parser AST.TypeCondition
typeCondition = namedType

-- * Values

-- This will try to pick the first type it can parse. If you are working with
-- explicit types use the `typedValue` parser.
value :: Parser AST.Value
value = AST.ValueVariable <$> variable
  -- TODO: Handle maxBound, Int32 in spec.
  <|> AST.ValueInt      <$> tok (signed decimal)
  <|> AST.ValueFloat    <$> tok (signed double)
  <|> AST.ValueBoolean  <$> booleanValue
  <|> AST.ValueString   <$> stringValue
  -- `true` and `false` have been tried before
  <|> AST.ValueEnum     <$> name
  <|> AST.ValueList     <$> listValue
  <|> AST.ValueObject   <$> objectValue
  <?> "value error!"

booleanValue :: Parser Bool
booleanValue = True  <$ tok "true"
   <|> False <$ tok "false"

-- TODO: Escape characters. Look at `jsstring_` in aeson package.
stringValue :: Parser AST.StringValue
stringValue = AST.StringValue <$> quotes (takeWhile (/= '"'))

-- Notice it can be empty
listValue :: Parser AST.ListValue
listValue = AST.ListValue <$> brackets (many value)

-- Notice it can be empty
objectValue :: Parser AST.ObjectValue
objectValue = AST.ObjectValue <$> braces (many objectField)

objectField :: Parser AST.ObjectField
objectField = AST.ObjectField <$> name <* tok ":" <*> value

-- * Directives

directives :: Parser [AST.Directive]
directives = many1 directive

directive :: Parser AST.Directive
directive = AST.Directive
  <$  tok "@"
  <*> name
  <*> optempty arguments

-- * Type Reference

type_ :: Parser AST.Type
type_ = AST.TypeList    <$> listType
    <|> AST.TypeNonNull <$> nonNullType
    <|> AST.TypeNamed   <$> namedType
    <?> "type_ error!"

namedType :: Parser AST.NamedType
namedType = AST.NamedType <$> name

listType :: Parser AST.ListType
listType = AST.ListType <$> brackets type_

nonNullType :: Parser AST.NonNullType
nonNullType = AST.NonNullTypeNamed <$> namedType <* tok "!"
          <|> AST.NonNullTypeList  <$> listType  <* tok "!"
          <?> "nonNullType error!"

-- * Type Definition

typeDefinition :: Parser AST.TypeDefinition
typeDefinition =
      AST.TypeDefinitionObject        <$> objectTypeDefinition
  <|> AST.TypeDefinitionInterface     <$> interfaceTypeDefinition
  <|> AST.TypeDefinitionUnion         <$> unionTypeDefinition
  <|> AST.TypeDefinitionScalar        <$> scalarTypeDefinition
  <|> AST.TypeDefinitionEnum          <$> enumTypeDefinition
  <|> AST.TypeDefinitionInputObject   <$> inputObjectTypeDefinition
  <|> AST.TypeDefinitionTypeExtension <$> typeExtensionDefinition
  <?> "typeDefinition error!"

objectTypeDefinition :: Parser AST.ObjectTypeDefinition
objectTypeDefinition = AST.ObjectTypeDefinition
  <$  tok "type"
  <*> name
  <*> optempty interfaces
  <*> fieldDefinitions

interfaces :: Parser AST.Interfaces
interfaces = tok "implements" *> many1 namedType

fieldDefinitions :: Parser [AST.FieldDefinition]
fieldDefinitions = braces $ many1 fieldDefinition

fieldDefinition :: Parser AST.FieldDefinition
fieldDefinition = AST.FieldDefinition
  <$> name
  <*> optempty argumentsDefinition
  <*  tok ":"
  <*> type_

argumentsDefinition :: Parser AST.ArgumentsDefinition
argumentsDefinition = parens $ many1 inputValueDefinition

interfaceTypeDefinition :: Parser AST.InterfaceTypeDefinition
interfaceTypeDefinition = AST.InterfaceTypeDefinition
  <$  tok "interface"
  <*> name
  <*> fieldDefinitions

unionTypeDefinition :: Parser AST.UnionTypeDefinition
unionTypeDefinition = AST.UnionTypeDefinition
  <$  tok "union"
  <*> name
  <*  tok "="
  <*> unionMembers

unionMembers :: Parser [AST.NamedType]
unionMembers = namedType `sepBy1` tok "|"

scalarTypeDefinition :: Parser AST.ScalarTypeDefinition
scalarTypeDefinition = AST.ScalarTypeDefinition
  <$  tok "scalar"
  <*> name

enumTypeDefinition :: Parser AST.EnumTypeDefinition
enumTypeDefinition = AST.EnumTypeDefinition
  <$  tok "enum"
  <*> name
  <*> enumValueDefinitions

enumValueDefinitions :: Parser [AST.EnumValueDefinition]
enumValueDefinitions = braces $ many1 enumValueDefinition

enumValueDefinition :: Parser AST.EnumValueDefinition
enumValueDefinition = AST.EnumValueDefinition <$> name

inputObjectTypeDefinition :: Parser AST.InputObjectTypeDefinition
inputObjectTypeDefinition = AST.InputObjectTypeDefinition
  <$  tok "input"
  <*> name
  <*> inputValueDefinitions

inputValueDefinitions :: Parser [AST.InputValueDefinition]
inputValueDefinitions = braces $ many1 inputValueDefinition

inputValueDefinition :: Parser AST.InputValueDefinition
inputValueDefinition = AST.InputValueDefinition
  <$> name
  <*  tok ":"
  <*> type_
  <*> optional defaultValue

typeExtensionDefinition :: Parser AST.TypeExtensionDefinition
typeExtensionDefinition = AST.TypeExtensionDefinition
  <$  tok "extend"
  <*> objectTypeDefinition

-- * Internal

tok :: Parser a -> Parser a
tok p = p <* whiteSpace

parens :: Parser a -> Parser a
parens = between "(" ")"

braces :: Parser a -> Parser a
braces = between "{" "}"

quotes :: Parser a -> Parser a
quotes = between "\"" "\""

brackets :: Parser a -> Parser a
brackets = between "[" "]"

between :: Parser Text -> Parser Text -> Parser a -> Parser a
between open close p = tok open *> p <* tok close

-- `empty` /= `pure mempty` for `Parser`.
optempty :: Monoid a => Parser a -> Parser a
optempty = option mempty

-- ** WhiteSpace
--
whiteSpace :: Parser ()
whiteSpace = peekChar >>= traverse_ (\c ->
  if isSpace c || c == ','
    then anyChar *> whiteSpace
    else when (c == '#') $ manyTill anyChar endOfLine *> whiteSpace)
