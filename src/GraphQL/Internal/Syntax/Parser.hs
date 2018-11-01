{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Parse text into GraphQL ASTs
module GraphQL.Internal.Syntax.Parser
  ( queryDocument
  , schemaDocument
  , value
  ) where

import Protolude hiding (option)

import Control.Applicative ((<|>), empty, many, optional)
import Control.Monad (fail)
import Data.Aeson.Parser (jstring)
import Data.Scientific (floatingOrInteger)
import Data.Text (find)
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.Text
  ( Parser
  , (<?>)
  , anyChar
  , char
  , match
  , many1
  , option
  , scan
  , scientific
  , sepBy1
  )
import qualified Data.Attoparsec.Internal.Types as AT

import qualified GraphQL.Internal.Syntax.AST as AST
import GraphQL.Internal.Syntax.Tokens (tok, whiteSpace)
import GraphQL.Internal.Name (nameParser)

getPos :: Parser Int
getPos = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

positioned :: Parser a -> Parser (a, (Int, Int))
positioned p = do
  start <- getPos
  content <- p
  end <- getPos
  return (content, (start, end))

-- * Document

queryDocument :: Parser AST.QueryDocument
queryDocument = whiteSpace *> do
  (content, pos) <- positioned (many1 definition <?> "query document error!")
  return $ AST.QueryDocument content pos

-- | Parser for a schema document.
schemaDocument :: Parser AST.SchemaDocument
schemaDocument = whiteSpace *> (AST.SchemaDocument <$> many1 typeDefinition) <?> "type document error"

definition :: Parser AST.Definition
definition = AST.DefinitionOperation <$> operationDefinition
         <|> AST.DefinitionFragment  <$> fragmentDefinition
         <?> "definition error!"

operationDefinition :: Parser AST.OperationDefinition
operationDefinition =
      AST.Query    <$ tok "query"    <*> node
  <|> AST.Mutation <$ tok "mutation" <*> node
  <|> (AST.AnonymousQuery <$> selectionSet)
  <?> "operationDefinition error!"

node :: Parser AST.Node
node = AST.Node <$> optional nameParser
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
variable = AST.Variable <$ tok "$" <*> nameParser

selectionSet :: Parser AST.SelectionSet
selectionSet = braces $ many1 selection

selection :: Parser AST.Selection
selection = AST.SelectionField <$> field
            -- Inline first to catch `on` case
        <|> AST.SelectionInlineFragment <$> inlineFragment
        <|> AST.SelectionFragmentSpread <$> fragmentSpread
        <?> "selection error!"

field :: Parser AST.Field
field = AST.Field <$> option empty (pure <$> alias)
                  <*> nameParser
                  <*> optempty arguments
                  <*> optempty directives
                  <*> optempty selectionSet

alias :: Parser AST.Alias
alias = nameParser <* tok ":"

arguments :: Parser [AST.Argument]
arguments = parens $ many1 argument

argument :: Parser AST.Argument
argument = AST.Argument <$> nameParser <* tok ":" <*> value

-- * Fragments

fragmentSpread :: Parser AST.FragmentSpread
-- TODO: Make sure it fails when `... on`.
-- See https://facebook.github.io/graphql/#FragmentSpread
fragmentSpread = AST.FragmentSpread
  <$  tok "..."
  <*> nameParser
  <*> optempty directives

-- InlineFragment tried first in order to guard against 'on' keyword
inlineFragment :: Parser AST.InlineFragment
inlineFragment = AST.InlineFragment
  <$  tok "..."
  <*> optional (tok "on" *> typeCondition)
  <*> optempty directives
  <*> selectionSet

fragmentDefinition :: Parser AST.FragmentDefinition
fragmentDefinition = AST.FragmentDefinition
  <$  tok "fragment"
  <*> nameParser
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
value = tok (AST.ValueVariable <$> (variable <?> "variable")
  <|> (number <?> "number")
  <|> AST.ValueNull     <$  tok "null"
  <|> AST.ValueBoolean  <$> (booleanValue <?> "booleanValue")
  <|> AST.ValueString   <$> (stringValue <?> "stringValue")
  -- `true` and `false` have been tried before
  <|> AST.ValueEnum     <$> (nameParser <?> "name")
  <|> AST.ValueList     <$> (listValue <?> "listValue")
  <|> AST.ValueObject   <$> (objectValue <?> "objectValue")
  <?> "value error!")
  where
    number =  do
      (numText, num) <- match (tok scientific)
      case (Data.Text.find (== '.') numText, floatingOrInteger num) of
        (Just _, Left r) -> pure (AST.ValueFloat r)
        (Just _, Right i) -> pure (AST.ValueFloat (fromIntegral i))
        -- TODO: Handle maxBound, Int32 in spec.
        (Nothing, Left r) -> pure (AST.ValueInt (floor r))
        (Nothing, Right i) -> pure (AST.ValueInt i)

booleanValue :: Parser Bool
booleanValue = True  <$ tok "true"
   <|> False <$ tok "false"

stringValue :: Parser AST.StringValue
stringValue = do
  parsed <- char '"' *> jstring_
  case unescapeText parsed of
    Left err -> fail err
    Right escaped -> pure (AST.StringValue escaped)
  where
    -- | Parse a string without a leading quote, ignoring any escaped characters.
    jstring_ :: Parser Text
    jstring_ = scan startState go <* anyChar

    startState = False
    go a c
      | a = Just False
      | c == '"' = Nothing
      | otherwise = let a' = c == backslash
                    in Just a'
      where backslash = '\\'

    -- | Unescape a string.
    --
    -- Turns out this is really tricky, so we're going to cheat by
    -- reconstructing a literal string (by putting quotes around it) and
    -- delegating all the hard work to Aeson.
    unescapeText str = A.parseOnly jstring ("\"" <> toS str <> "\"")

-- Notice it can be empty
listValue :: Parser AST.ListValue
listValue = AST.ListValue <$> brackets (many value)

-- Notice it can be empty
objectValue :: Parser AST.ObjectValue
objectValue = AST.ObjectValue <$> braces (many (objectField <?> "objectField"))

objectField :: Parser AST.ObjectField
objectField = AST.ObjectField <$> nameParser <* tok ":" <*> value

-- * Directives

directives :: Parser [AST.Directive]
directives = many1 directive

directive :: Parser AST.Directive
directive = AST.Directive
  <$  tok "@"
  <*> nameParser
  <*> optempty arguments

-- * Type Reference

type_ :: Parser AST.GType
type_ = AST.TypeList    <$> listType
    <|> AST.TypeNonNull <$> nonNullType
    <|> AST.TypeNamed   <$> namedType
    <?> "type_ error!"

namedType :: Parser AST.NamedType
namedType = AST.NamedType <$> nameParser

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
  <*> nameParser
  <*> optempty interfaces
  <*> fieldDefinitions

interfaces :: Parser AST.Interfaces
interfaces = tok "implements" *> many1 namedType

fieldDefinitions :: Parser [AST.FieldDefinition]
fieldDefinitions = braces $ many1 fieldDefinition

fieldDefinition :: Parser AST.FieldDefinition
fieldDefinition = AST.FieldDefinition
  <$> nameParser
  <*> optempty argumentsDefinition
  <*  tok ":"
  <*> type_

argumentsDefinition :: Parser AST.ArgumentsDefinition
argumentsDefinition = parens $ many1 inputValueDefinition

interfaceTypeDefinition :: Parser AST.InterfaceTypeDefinition
interfaceTypeDefinition = AST.InterfaceTypeDefinition
  <$  tok "interface"
  <*> nameParser
  <*> fieldDefinitions

unionTypeDefinition :: Parser AST.UnionTypeDefinition
unionTypeDefinition = AST.UnionTypeDefinition
  <$  tok "union"
  <*> nameParser
  <*  tok "="
  <*> unionMembers

unionMembers :: Parser [AST.NamedType]
unionMembers = namedType `sepBy1` tok "|"

scalarTypeDefinition :: Parser AST.ScalarTypeDefinition
scalarTypeDefinition = AST.ScalarTypeDefinition
  <$  tok "scalar"
  <*> nameParser

enumTypeDefinition :: Parser AST.EnumTypeDefinition
enumTypeDefinition = AST.EnumTypeDefinition
  <$  tok "enum"
  <*> nameParser
  <*> enumValueDefinitions

enumValueDefinitions :: Parser [AST.EnumValueDefinition]
enumValueDefinitions = braces $ many1 enumValueDefinition

enumValueDefinition :: Parser AST.EnumValueDefinition
enumValueDefinition = AST.EnumValueDefinition <$> nameParser

inputObjectTypeDefinition :: Parser AST.InputObjectTypeDefinition
inputObjectTypeDefinition = AST.InputObjectTypeDefinition
  <$  tok "input"
  <*> nameParser
  <*> inputValueDefinitions

inputValueDefinitions :: Parser [AST.InputValueDefinition]
inputValueDefinitions = braces $ many1 inputValueDefinition

inputValueDefinition :: Parser AST.InputValueDefinition
inputValueDefinition = AST.InputValueDefinition
  <$> nameParser
  <*  tok ":"
  <*> type_
  <*> optional defaultValue

typeExtensionDefinition :: Parser AST.TypeExtensionDefinition
typeExtensionDefinition = AST.TypeExtensionDefinition
  <$  tok "extend"
  <*> objectTypeDefinition

-- * Internal

parens :: Parser a -> Parser a
parens = between "(" ")"

braces :: Parser a -> Parser a
braces = between "{" "}"

brackets :: Parser a -> Parser a
brackets = between "[" "]"

between :: Parser Text -> Parser Text -> Parser a -> Parser a
between open close p = tok open *> p <* tok close

-- `empty` /= `pure mempty` for `Parser`.
optempty :: Monoid a => Parser a -> Parser a
optempty = option mempty
