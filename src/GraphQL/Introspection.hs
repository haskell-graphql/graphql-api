{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module GraphQL.Introspection
  ( SchemaField
  , TypeField
  , schema
  , type_
  , serialize
  ) where

import Protolude hiding (TypeError, Enum)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as T

import GraphQL (SchemaRoot(..))
import GraphQL.API
import GraphQL.Resolver
import GraphQL.Internal.Name (getName, unName)
import GraphQL.Internal.Schema

import qualified GraphQL.Internal.Syntax.AST     as AST
import qualified GraphQL.Internal.Syntax.Encoder as E

-- See http://facebook.github.io/graphql/June2018/#sec-Schema-Introspection
type Schema__ = Object "__Schema" '[]
  '[ Field "types" (List Type__)
   , Field "queryType" Type__
   , Field "mutationType" Type__
   ]

type Type__ = Object "__Type" '[]
  '[ Field "kind" TypeKind__
   , Field "name" Text
   , Field "fields" (Maybe (List Field__))
   , Field "enumValues" (Maybe (List EnumValue__))
   , Field "inputFields" (Maybe (List InputValue__))
   ]

type Field__ = Object "__Field" '[]
  '[ Field "name" Text
   , Field "args" (List InputValue__)
   ]

type EnumValue__ = Object "__EnumValue" '[]
  '[ Field "name" Text
   ]

type InputValue__ = Object "__InputValue" '[]
  '[ Field "name" Text
   ]

data TypeKind = SCALAR
              | OBJECT
              | INTERFACE
              | UNION
              | ENUM
              | INPUT_OBJECT
              | LIST
              | NON_NULL
              deriving (Show, Eq, Generic)
instance GraphQLEnum TypeKind

type TypeKind__ = Enum "__TypeKind" TypeKind

type SchemaField = Field "__schema" Schema__
type TypeField   = Argument "name" Text :> Field "__type" Type__

data SchemaDefinition = SchemaDefinition ObjectTypeDefinition ObjectTypeDefinition

schemaDefinedTypes :: SchemaDefinition -> Map Name TypeDefinition
schemaDefinedTypes (SchemaDefinition queries mutations) =
  Map.filterWithKey defined $ getDefinedTypes queries <> getDefinedTypes mutations
    where
      defined name _ = not $ reserved name

reserved :: Name -> Bool
reserved name = "__" `T.isPrefixOf` unName name

serialize :: forall s h q m.
  ( s ~ SchemaRoot h q m
  , HasObjectDefinition q
  , HasObjectDefinition m
  ) => Either SchemaError Text
serialize = do
  queries   <- getDefinition @q
  mutations <- getDefinition @m
  let definitions = ordNub $ collectDefinitions queries <> collectDefinitions mutations

  return $
    E.schemaDocument (AST.SchemaDocument $ map typeDefinitionToAST definitions) <>
    ",schema{query:" <> unName (getName queries) <> ",mutation:" <> unName (getName mutations) <> "}"

collectDefinitions :: ObjectTypeDefinition -> [TypeDefinition]
collectDefinitions = visitObject
  where
    visitObject (ObjectTypeDefinition name interfaces fields) =
      if reserved name
        then []
        else
          let fields' = NonEmpty.fromList $ NonEmpty.filter (not . reserved . getName) fields
          in TypeDefinitionObject (ObjectTypeDefinition name interfaces fields') : concatMap visitField fields'

    visitField (FieldDefinition _ args out) =
      visitType out <> concatMap visitArg args

    visitArg (ArgumentDefinition _ input _) = case unAnnotatedType input of
      DefinedInputType (InputTypeDefinitionObject inputObject) -> visitInputObjectType inputObject
      _ -> []

    visitType t = case unAnnotatedType t of
      DefinedType (TypeDefinitionObject object) -> visitObject object
      DefinedType definition -> [definition]
      _ -> []

    visitInputObjectType inputObject@(InputObjectTypeDefinition _ fields) =
      TypeDefinitionInputObject inputObject : concatMap visitInputObjectField fields

    visitInputObjectField (InputObjectFieldDefinition _ input _) = case unAnnotatedType input of
      DefinedInputType (InputTypeDefinitionObject object) -> visitInputObjectType object
      _ -> []

unAnnotatedType :: AnnotatedType t -> t
unAnnotatedType (TypeNamed t) = t
unAnnotatedType (TypeList (ListType t)) = unAnnotatedType t
unAnnotatedType (TypeNonNull (NonNullTypeNamed t)) = t
unAnnotatedType (TypeNonNull (NonNullTypeList (ListType t))) = unAnnotatedType t

{-
 - Schema => AST conversions
 -
 - These are generally used to take a validated schema obtained from e.g. `getDefinition @type`
 - and produce a schema document suitable for consumption by an external client
 -}

typeDefinitionToAST :: TypeDefinition -> AST.TypeDefinition
typeDefinitionToAST (TypeDefinitionObject o     )   = AST.TypeDefinitionObject $ objectTypeDefinitionToAST o
typeDefinitionToAST (TypeDefinitionInputObject o)   = AST.TypeDefinitionInputObject $ inputObjectTypeDefinitionToAST o
typeDefinitionToAST (TypeDefinitionInterface _)     = panic "interface"
typeDefinitionToAST (TypeDefinitionUnion _)         = panic "union"
typeDefinitionToAST (TypeDefinitionScalar _)        = panic "scalar"
typeDefinitionToAST (TypeDefinitionEnum _)          = panic "enum"
typeDefinitionToAST (TypeDefinitionTypeExtension _) = panic "extension"

objectTypeDefinitionToAST :: ObjectTypeDefinition -> AST.ObjectTypeDefinition
objectTypeDefinitionToAST (ObjectTypeDefinition name interfaces fields) =
  AST.ObjectTypeDefinition name (map interfaceTypeDefinitionToAST interfaces) (NonEmpty.toList $ map fieldDefinitionToAST fields)

inputObjectTypeDefinitionToAST :: InputObjectTypeDefinition -> AST.InputObjectTypeDefinition
inputObjectTypeDefinitionToAST (InputObjectTypeDefinition name fields) =
  AST.InputObjectTypeDefinition name (NonEmpty.toList $ map inputObjectFieldDefinitionToAST fields)

interfaceTypeDefinitionToAST :: InterfaceTypeDefinition -> AST.NamedType
interfaceTypeDefinitionToAST (InterfaceTypeDefinition name _) = AST.NamedType name

fieldDefinitionToAST :: FieldDefinition -> AST.FieldDefinition
fieldDefinitionToAST (FieldDefinition name args out) = AST.FieldDefinition name (map argToInputValue args) (typeToAST out)

argToInputValue :: ArgumentDefinition -> AST.InputValueDefinition
argToInputValue (ArgumentDefinition name annotatedInput _) = AST.InputValueDefinition name (inputTypeToAST annotatedInput) Nothing -- FIXME

inputObjectFieldDefinitionToAST :: InputObjectFieldDefinition -> AST.InputValueDefinition
inputObjectFieldDefinitionToAST (InputObjectFieldDefinition name annotatedInput _) = AST.InputValueDefinition name (inputTypeToAST annotatedInput) Nothing -- FIXME

typeToAST :: AnnotatedType GType -> AST.GType
typeToAST (TypeNamed t) =
  -- AST.TypeNamed $ AST.NamedType $ getName t
  AST.TypeNonNull $ AST.NonNullTypeNamed $ AST.NamedType $ getName t
typeToAST (TypeList (ListType t)) =
  -- AST.TypeList $ AST.ListType $ AST.TypeNamed $ AST.NamedType $ getName t
  AST.TypeNonNull $ AST.NonNullTypeList $ AST.ListType $
  -- AST.TypeNamed $ AST.NamedType $ getName t
  AST.TypeNonNull $ AST.NonNullTypeNamed $ AST.NamedType $ getName t
typeToAST (TypeNonNull (NonNullTypeNamed t)) =
  AST.TypeNonNull $ AST.NonNullTypeNamed $ AST.NamedType $ getName t
typeToAST (TypeNonNull (NonNullTypeList (ListType t))) =
  AST.TypeNonNull $ AST.NonNullTypeList $ AST.ListType $
  -- AST.TypeNamed $ AST.NamedType $ getName t
  AST.TypeNonNull $ AST.NonNullTypeNamed $ AST.NamedType $ getName t

inputTypeToAST :: AnnotatedType InputType -> AST.GType
inputTypeToAST (TypeNamed t) =
  AST.TypeNamed $ AST.NamedType $ getName t
inputTypeToAST (TypeList (ListType t)) =
  AST.TypeNonNull $ AST.NonNullTypeList $ AST.ListType $
  AST.TypeNamed $ AST.NamedType $ getName t
inputTypeToAST (TypeNonNull (NonNullTypeNamed t)) =
  AST.TypeNonNull $ AST.NonNullTypeNamed $ AST.NamedType $ getName t
inputTypeToAST (TypeNonNull (NonNullTypeList (ListType t))) =
  AST.TypeNonNull $ AST.NonNullTypeList $ AST.ListType $
  AST.TypeNamed $ AST.NamedType $ getName t

schema :: forall s m queries mutations.
  ( s ~ SchemaRoot m queries mutations
  , HasObjectDefinition queries
  , HasObjectDefinition mutations
  , Monad m
  ) => Handler m Schema__
schema = do
  let Right queries   = getDefinition @queries
      Right mutations = getDefinition @mutations
      types = schemaDefinedTypes $ SchemaDefinition queries mutations
  pure
    $ pure (map typeHandler $ Map.elems types)
    :<> objectTypeHandler queries
    :<> objectTypeHandler mutations

type_ :: forall s m queries mutations.
  ( s ~ SchemaRoot m queries mutations
  , HasObjectDefinition queries
  , HasObjectDefinition mutations
  , Monad m
  ) => Text -> Handler m Type__
type_ name = do
  let Right queries   = getDefinition @queries
      Right mutations = getDefinition @mutations
      types = Map.mapKeys unName $ schemaDefinedTypes $ SchemaDefinition queries mutations
  case Map.lookup name types of
    Just t -> typeHandler t
    Nothing -> panic "failed to find type"

typeHandler :: Monad m => TypeDefinition -> Handler m Type__
typeHandler (TypeDefinitionObject object) = objectTypeHandler object
typeHandler (TypeDefinitionInterface interface) = interfaceTypeHandler interface
typeHandler (TypeDefinitionUnion union) = unionTypeHandler union
typeHandler (TypeDefinitionScalar scalar) = scalarTypeHandler scalar
typeHandler (TypeDefinitionEnum enum) = enumTypeHandler enum
typeHandler (TypeDefinitionInputObject input) = inputObjectTypeHandler input
typeHandler (TypeDefinitionTypeExtension ex) = typeExtensionTypeHandler ex

objectTypeHandler :: Monad m => ObjectTypeDefinition -> Handler m Type__
objectTypeHandler (ObjectTypeDefinition name _ fields) = pure
    $ pure OBJECT
  :<> pure (unName name)
  :<> pure (Just . pure $ map fieldHandler $ NonEmpty.toList fields)
  :<> pure Nothing
  :<> pure Nothing

enumTypeHandler :: Monad m => EnumTypeDefinition -> Handler m Type__
enumTypeHandler (EnumTypeDefinition name values) = pure
    $ pure ENUM
  :<> pure (unName name)
  :<> pure Nothing
  :<> pure (Just . pure $ map (pure . pure . unName . getName) values)
  :<> pure Nothing

unionTypeHandler :: Monad m => UnionTypeDefinition -> Handler m Type__
unionTypeHandler (UnionTypeDefinition name _) = pure
    $ pure UNION
  :<> pure (unName name)
  :<> pure Nothing
  :<> pure Nothing
  :<> pure Nothing

interfaceTypeHandler :: Monad m => InterfaceTypeDefinition -> Handler m Type__
interfaceTypeHandler (InterfaceTypeDefinition name fields) = pure
    $ pure INTERFACE
  :<> pure (unName name)
  :<> pure (Just . pure $ map fieldHandler $ NonEmpty.toList fields)
  :<> pure Nothing
  :<> pure Nothing

scalarTypeHandler :: Monad m => ScalarTypeDefinition -> Handler m Type__
scalarTypeHandler (ScalarTypeDefinition name) = pure
    $ pure SCALAR
  :<> pure (unName name)
  :<> pure Nothing
  :<> pure Nothing
  :<> pure Nothing

inputObjectTypeHandler :: Monad m => InputObjectTypeDefinition -> Handler m Type__
inputObjectTypeHandler (InputObjectTypeDefinition name fields) = pure
    $ pure INPUT_OBJECT
  :<> pure (unName name)
  :<> pure Nothing
  :<> pure Nothing
  :<> pure (Just . pure $ map (pure . pure . unName . getName) $ NonEmpty.toList fields)

typeExtensionTypeHandler :: Monad m => TypeExtensionDefinition -> Handler m Type__
typeExtensionTypeHandler (TypeExtensionDefinition obj) = objectTypeHandler obj

fieldHandler :: Monad m => FieldDefinition -> Handler m Field__
fieldHandler (FieldDefinition name args _) = pure
    $ pure (unName name)
  :<> pure (map (pure . pure . unName . getName) args)