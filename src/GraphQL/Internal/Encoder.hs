{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module GraphQL.Internal.Encoder
  ( document
  ) where

import Protolude hiding (Type, intercalate)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (Monoid, mconcat, mempty)
#endif

import Data.Text (Text, cons, intercalate, pack, snoc)

import qualified GraphQL.Internal.AST as AST

-- * Document

-- TODO: Use query shorthand
document :: AST.Document -> Text
document (AST.Document defs) = (`snoc` '\n') . mconcat $ definition <$> defs

definition :: AST.Definition -> Text
definition (AST.DefinitionOperation x) = operationDefinition x
definition (AST.DefinitionFragment  x) = fragmentDefinition x
definition (AST.DefinitionType      x) = typeDefinition x

operationDefinition :: AST.OperationDefinition -> Text
operationDefinition (AST.Query    n) = "query "    <> node n
operationDefinition (AST.Mutation n) = "mutation " <> node n

node :: AST.Node -> Text
node (AST.Node name vds ds ss) =
       name
    <> optempty variableDefinitions vds
    <> optempty directives ds
    <> selectionSet ss

variableDefinitions :: [AST.VariableDefinition] -> Text
variableDefinitions = parensCommas variableDefinition

variableDefinition :: AST.VariableDefinition -> Text
variableDefinition (AST.VariableDefinition var ty dv) =
    variable var <> ":" <> type_ ty <> maybe mempty defaultValue dv

defaultValue :: AST.DefaultValue -> Text
defaultValue val = "=" <> value val

variable :: AST.Variable -> Text
variable (AST.Variable name) = "$" <> name

selectionSet :: AST.SelectionSet -> Text
selectionSet = bracesCommas selection

selection :: AST.Selection -> Text
selection (AST.SelectionField          x) = field x
selection (AST.SelectionInlineFragment x) = inlineFragment x
selection (AST.SelectionFragmentSpread x) = fragmentSpread x

field :: AST.Field -> Text
field (AST.Field alias name args ds ss) =
       optempty (`snoc` ':') alias
    <> name
    <> optempty arguments args
    <> optempty directives ds
    <> optempty selectionSet ss

arguments :: [AST.Argument] -> Text
arguments = parensCommas argument

argument :: AST.Argument -> Text
argument (AST.Argument name v) = name <> ":" <> value v

-- * Fragments

fragmentSpread :: AST.FragmentSpread -> Text
fragmentSpread (AST.FragmentSpread name ds) =
    "..." <> name <> optempty directives ds

inlineFragment :: AST.InlineFragment -> Text
inlineFragment (AST.InlineFragment (AST.NamedType tc) ds ss) =
    "... on " <> tc
              <> optempty directives ds
              <> optempty selectionSet ss

fragmentDefinition :: AST.FragmentDefinition -> Text
fragmentDefinition (AST.FragmentDefinition name (AST.NamedType tc) ds ss) =
    "fragment " <> name <> " on " <> tc
                <> optempty directives ds
                <> selectionSet ss

-- * Values

value :: AST.Value -> Text
value (AST.ValueVariable x) = variable x
-- TODO: This will be replaced with `decimal` Buidler
value (AST.ValueInt      x) = pack $ show x
-- TODO: This will be replaced with `decimal` Buidler
value (AST.ValueFloat    x) = pack $ show x
value (AST.ValueBoolean  x) = booleanValue x
value (AST.ValueString   x) = stringValue x
value (AST.ValueEnum     x) = x
value (AST.ValueList     x) = listValue x
value (AST.ValueObject   x) = objectValue x

booleanValue :: Bool -> Text
booleanValue True  = "true"
booleanValue False = "false"

-- TODO: Escape characters
stringValue :: AST.StringValue -> Text
stringValue (AST.StringValue v) = quotes v

listValue :: AST.ListValue -> Text
listValue (AST.ListValue vs) = bracketsCommas value vs

objectValue :: AST.ObjectValue -> Text
objectValue (AST.ObjectValue ofs) = bracesCommas objectField ofs

objectField :: AST.ObjectField -> Text
objectField (AST.ObjectField name v) = name <> ":" <> value v

-- * Directives

directives :: [AST.Directive] -> Text
directives = spaces directive

directive :: AST.Directive -> Text
directive (AST.Directive name args) = "@" <> name <> optempty arguments args

-- * Type Reference

type_ :: AST.Type -> Text
type_ (AST.TypeNamed (AST.NamedType x)) = x
type_ (AST.TypeList x) = listType x
type_ (AST.TypeNonNull x) = nonNullType x

namedType :: AST.NamedType -> Text
namedType (AST.NamedType name) = name

listType :: AST.ListType -> Text
listType (AST.ListType ty) = brackets (type_ ty)

nonNullType :: AST.NonNullType -> Text
nonNullType (AST.NonNullTypeNamed (AST.NamedType x)) = x <> "!"
nonNullType (AST.NonNullTypeList  x) = listType x <> "!"

typeDefinition :: AST.TypeDefinition -> Text
typeDefinition (AST.TypeDefinitionObject        x) = objectTypeDefinition x
typeDefinition (AST.TypeDefinitionInterface     x) = interfaceTypeDefinition x
typeDefinition (AST.TypeDefinitionUnion         x) = unionTypeDefinition x
typeDefinition (AST.TypeDefinitionScalar        x) = scalarTypeDefinition x
typeDefinition (AST.TypeDefinitionEnum          x) = enumTypeDefinition x
typeDefinition (AST.TypeDefinitionInputObject   x) = inputObjectTypeDefinition x
typeDefinition (AST.TypeDefinitionTypeExtension x) = typeExtensionDefinition x

objectTypeDefinition :: AST.ObjectTypeDefinition -> Text
objectTypeDefinition (AST.ObjectTypeDefinition name ifaces fds) =
    "type " <> name
            <> optempty (spaced . interfaces) ifaces
            <> optempty fieldDefinitions fds

interfaces :: AST.Interfaces -> Text
interfaces = ("implements " <>) . spaces namedType

fieldDefinitions :: [AST.FieldDefinition] -> Text
fieldDefinitions = bracesCommas fieldDefinition

fieldDefinition :: AST.FieldDefinition -> Text
fieldDefinition (AST.FieldDefinition name args ty) =
    name <> optempty argumentsDefinition args
         <> ":"
         <> type_ ty

argumentsDefinition :: AST.ArgumentsDefinition -> Text
argumentsDefinition = parensCommas inputValueDefinition

interfaceTypeDefinition :: AST.InterfaceTypeDefinition -> Text
interfaceTypeDefinition (AST.InterfaceTypeDefinition name fds) =
    "interface " <> name <> fieldDefinitions fds

unionTypeDefinition :: AST.UnionTypeDefinition -> Text
unionTypeDefinition (AST.UnionTypeDefinition name ums) =
    "union " <> name <> "=" <> unionMembers ums

unionMembers :: [AST.NamedType] -> Text
unionMembers = intercalate "|" . fmap namedType

scalarTypeDefinition :: AST.ScalarTypeDefinition -> Text
scalarTypeDefinition (AST.ScalarTypeDefinition name) = "scalar " <> name

enumTypeDefinition :: AST.EnumTypeDefinition -> Text
enumTypeDefinition (AST.EnumTypeDefinition name evds) =
    "enum " <> name
            <> bracesCommas enumValueDefinition evds

enumValueDefinition :: AST.EnumValueDefinition -> Text
enumValueDefinition (AST.EnumValueDefinition name) = name

inputObjectTypeDefinition :: AST.InputObjectTypeDefinition -> Text
inputObjectTypeDefinition (AST.InputObjectTypeDefinition name ivds) =
    "input " <> name <> inputValueDefinitions ivds

inputValueDefinitions :: [AST.InputValueDefinition] -> Text
inputValueDefinitions = bracesCommas inputValueDefinition

inputValueDefinition :: AST.InputValueDefinition -> Text
inputValueDefinition (AST.InputValueDefinition name ty dv) =
    name <> ":" <> type_ ty <> maybe mempty defaultValue dv

typeExtensionDefinition :: AST.TypeExtensionDefinition -> Text
typeExtensionDefinition (AST.TypeExtensionDefinition otd) =
    "extend " <> objectTypeDefinition otd

-- * Internal

spaced :: Text -> Text
spaced = cons '\SP'

between :: Char -> Char -> Text -> Text
between open close = cons open . (`snoc` close)

parens :: Text -> Text
parens = between '(' ')'

brackets :: Text -> Text
brackets = between '[' ']'

braces :: Text -> Text
braces = between '{' '}'

quotes :: Text -> Text
quotes = between '"' '"'

spaces :: (a -> Text) -> [a] -> Text
spaces f = intercalate "\SP" . fmap f

parensCommas :: (a -> Text) -> [a] -> Text
parensCommas f = parens . intercalate "," . fmap f

bracketsCommas :: (a -> Text) -> [a] -> Text
bracketsCommas f = brackets . intercalate "," . fmap f

bracesCommas :: (a -> Text) -> [a] -> Text
bracesCommas f = braces . intercalate "," . fmap f

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs
