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
  , InputObjectTypeDefinition(..)
  , InputObjectFieldDefinition(..)
  -- * Using existing types
  , AnnotatedType(..)
  , ListType(..)
  , NonNullType(..)
  , DefinesTypes(..)
  , doesFragmentTypeApply
  -- * The schema
  , Schema
  , makeSchema
  , lookupType
  ) where

import Protolude hiding (Type)

import qualified Data.Map as Map
import GraphQL.Value (Value)
import GraphQL.Internal.Name (HasName(..), Name)

-- | An entire GraphQL schema.
--
-- This is very much a work in progress. Currently, the only thing we provide
-- is a dictionary mapping type names to their definitions.
newtype Schema = Schema (Map Name TypeDefinition) deriving (Eq, Ord, Show)

-- | Create a schema from the root object.
--
-- This is technically an insufficient API, since not all types in a schema
-- need to be reachable from a single root object. However, it's a start.
makeSchema :: ObjectTypeDefinition -> Schema
makeSchema = Schema . getDefinedTypes

-- | Find the type with the given name in the schema.
lookupType :: Schema -> Name -> Maybe TypeDefinition
lookupType (Schema schema) name = Map.lookup name schema

-- XXX: Use the built-in NonEmptyList in Haskell
newtype NonEmptyList a = NonEmptyList [a] deriving (Eq, Ord, Show, Functor, Foldable)

-- | A thing that defines types. Excludes definitions of input types.
class DefinesTypes t where
  -- | Get the types defined by @t@
  --
  -- TODO: This ignores whether a value can define multiple types with the
  -- same name, and further admits the possibility that the name embedded in
  -- the type definition does not match the name in the returned dictionary.
  -- jml would like to have a schema validation phase that eliminates one or
  -- both of these possibilities.
  --
  -- Also pretty much works because we've inlined all our type definitions.
  getDefinedTypes :: t -> Map Name TypeDefinition

data AnnotatedType t = TypeNamed t
                     | TypeList (ListType t)
                     | TypeNonNull (NonNullType t)
                     deriving (Eq, Ord, Show)

-- | Get the type that is being annotated.
getAnnotatedType :: AnnotatedType t -> t
getAnnotatedType (TypeNamed t) = t
getAnnotatedType (TypeList (ListType t)) = getAnnotatedType t
getAnnotatedType (TypeNonNull (NonNullTypeNamed t)) = t
getAnnotatedType (TypeNonNull (NonNullTypeList (ListType t))) = getAnnotatedType t

instance HasName t => HasName (AnnotatedType t) where
  getName = getName . getAnnotatedType

newtype ListType t = ListType (AnnotatedType t) deriving (Eq, Ord, Show)

data NonNullType t = NonNullTypeNamed t
                   | NonNullTypeList  (ListType t)
                   deriving (Eq, Ord, Show)

data Type = DefinedType TypeDefinition | BuiltinType Builtin deriving (Eq, Ord, Show)

instance DefinesTypes Type where
  getDefinedTypes (BuiltinType _) = mempty
  getDefinedTypes (DefinedType t) = getDefinedTypes t

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
                      deriving (Eq, Ord, Show)

instance HasName TypeDefinition where
  getName (TypeDefinitionObject x) = getName x
  getName (TypeDefinitionInterface x) = getName x
  getName (TypeDefinitionUnion x) = getName x
  getName (TypeDefinitionScalar x) = getName x
  getName (TypeDefinitionEnum x) = getName x
  getName (TypeDefinitionInputObject x) = getName x
  getName (TypeDefinitionTypeExtension x) = getName x

instance DefinesTypes TypeDefinition where
  getDefinedTypes defn =
    case defn of
      TypeDefinitionObject x -> getDefinedTypes x
      TypeDefinitionInterface x -> getDefinedTypes x
      TypeDefinitionUnion x -> getDefinedTypes x
      TypeDefinitionScalar x  -> getDefinedTypes x
      TypeDefinitionEnum x -> getDefinedTypes x
      TypeDefinitionInputObject _ -> mempty
      TypeDefinitionTypeExtension _ ->
        panic "TODO: we should remove the 'extend' behaviour entirely"

data ObjectTypeDefinition = ObjectTypeDefinition Name Interfaces (NonEmptyList FieldDefinition)
                            deriving (Eq, Ord, Show)

instance HasName ObjectTypeDefinition where
  getName (ObjectTypeDefinition name _ _) = name

instance DefinesTypes ObjectTypeDefinition where
  getDefinedTypes obj@(ObjectTypeDefinition name interfaces fields) =
    Map.singleton name (TypeDefinitionObject obj) <>
    foldMap getDefinedTypes interfaces <>
    foldMap getDefinedTypes fields

type Interfaces = [InterfaceTypeDefinition]

data FieldDefinition = FieldDefinition Name [ArgumentDefinition] (AnnotatedType Type)
                       deriving (Eq, Ord, Show)

instance HasName FieldDefinition where
  getName (FieldDefinition name _ _) = name

instance DefinesTypes FieldDefinition where
  getDefinedTypes (FieldDefinition _ _ retVal) = getDefinedTypes (getAnnotatedType retVal)

data ArgumentDefinition = ArgumentDefinition Name (AnnotatedType InputType) (Maybe DefaultValue)
                          deriving (Eq, Ord, Show)

instance HasName ArgumentDefinition where
  getName (ArgumentDefinition name _ _) = name

data InterfaceTypeDefinition = InterfaceTypeDefinition Name (NonEmptyList FieldDefinition)
                               deriving (Eq, Ord, Show)

instance HasName InterfaceTypeDefinition where
  getName (InterfaceTypeDefinition name _) = name

instance DefinesTypes InterfaceTypeDefinition where
  getDefinedTypes i@(InterfaceTypeDefinition name fields) = Map.singleton name (TypeDefinitionInterface i) <> foldMap getDefinedTypes fields

data UnionTypeDefinition = UnionTypeDefinition Name (NonEmptyList ObjectTypeDefinition)
                           deriving (Eq, Ord, Show)

instance HasName UnionTypeDefinition where
  getName (UnionTypeDefinition name _) = name

instance DefinesTypes UnionTypeDefinition where
  getDefinedTypes defn@(UnionTypeDefinition name objs) =
    Map.singleton name (TypeDefinitionUnion defn) <>
    foldMap getDefinedTypes objs

newtype ScalarTypeDefinition = ScalarTypeDefinition Name
                             deriving (Eq, Ord, Show)

instance HasName ScalarTypeDefinition where
  getName (ScalarTypeDefinition name) = name

instance DefinesTypes ScalarTypeDefinition where
  getDefinedTypes defn = Map.singleton (getName defn) (TypeDefinitionScalar defn)

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
  | GID deriving (Eq, Ord, Show)

instance HasName Builtin where
  getName GInt = "Int"
  getName GBool = "Boolean"
  getName GString = "String"
  getName GFloat = "Float"
  getName GID = "ID"

data EnumTypeDefinition = EnumTypeDefinition Name [EnumValueDefinition]
                          deriving (Eq, Ord, Show)

instance HasName EnumTypeDefinition where
  getName (EnumTypeDefinition name _) = name

instance DefinesTypes EnumTypeDefinition where
  getDefinedTypes enum = Map.singleton (getName enum) (TypeDefinitionEnum enum)

newtype EnumValueDefinition = EnumValueDefinition Name
                              deriving (Eq, Ord, Show)

instance HasName EnumValueDefinition where
  getName (EnumValueDefinition name) = name

data InputObjectTypeDefinition = InputObjectTypeDefinition Name (NonEmptyList InputObjectFieldDefinition)
                                 deriving (Eq, Ord, Show)

instance HasName InputObjectTypeDefinition where
  getName (InputObjectTypeDefinition name _) = name

data InputObjectFieldDefinition = InputObjectFieldDefinition Name (AnnotatedType InputType) (Maybe DefaultValue)
                                  deriving (Eq, Ord, Show) -- XXX: spec is unclear about default value for input object field definitions

instance HasName InputObjectFieldDefinition where
  getName (InputObjectFieldDefinition name _ _) = name

newtype TypeExtensionDefinition = TypeExtensionDefinition ObjectTypeDefinition
                                  deriving (Eq, Ord, Show)

instance HasName TypeExtensionDefinition where
  getName (TypeExtensionDefinition obj) = getName obj

data InputType = DefinedInputType InputTypeDefinition | BuiltinInputType Builtin deriving (Eq, Ord, Show)

instance HasName InputType where
  getName (DefinedInputType x) = getName x
  getName (BuiltinInputType x) = getName x

data InputTypeDefinition
  = InputTypeDefinitionObject        InputObjectTypeDefinition
  | InputTypeDefinitionScalar        ScalarTypeDefinition
  | InputTypeDefinitionEnum          EnumTypeDefinition
  deriving (Eq, Ord, Show)

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


-- | Does the given object type match the given type condition.
--
-- See <https://facebook.github.io/graphql/#sec-Field-Collection>
--
-- @
-- DoesFragmentTypeApply(objectType, fragmentType)
--   If fragmentType is an Object Type:
--     if objectType and fragmentType are the same type, return true, otherwise return false.
--   If fragmentType is an Interface Type:
--     if objectType is an implementation of fragmentType, return true otherwise return false.
--   If fragmentType is a Union:
--     if objectType is a possible type of fragmentType, return true otherwise return false.
-- @
doesFragmentTypeApply :: ObjectTypeDefinition -> TypeDefinition -> Bool
doesFragmentTypeApply objectType fragmentType =
  case fragmentType of
    TypeDefinitionObject obj -> obj == objectType
    TypeDefinitionInterface interface -> objectType `implements` interface
    TypeDefinitionUnion union -> objectType `branchOf` union
    _ -> False
  where
    implements (ObjectTypeDefinition _ interfaces _) int = int `elem` interfaces
    branchOf obj (UnionTypeDefinition _ (NonEmptyList branches)) = obj `elem` branches
