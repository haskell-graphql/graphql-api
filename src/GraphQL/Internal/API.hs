{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Define a GraphQL schema with Haskell types
module GraphQL.Internal.API
  ( Object
  , Field
  , Argument
  , Union
  , List
  , Enum
  , GraphQLEnum(..)
  , Interface
  , (:>)(..)
  , Defaultable(..)
  , HasAnnotatedType(..)
  , HasAnnotatedInputType
  , HasObjectDefinition(..)
  , getArgumentDefinition
  , SchemaError(..)
  , nameFromSymbol
  -- | Exported for testing.
  , getFieldDefinition
  , getInterfaceDefinition
  , getAnnotatedInputType
  ) where

import Protolude hiding (Enum, TypeError, (<>))

import Data.Semigroup ((<>))
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Generics ((:*:)(..))
import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..))
import GHC.Types (Type)

import qualified GraphQL.Internal.Schema as Schema
import qualified GraphQL.Internal.Name as Name
import GraphQL.Internal.Name (Name, NameError)
import GraphQL.Internal.API.Enum (GraphQLEnum(..))
import GraphQL.Internal.Output (GraphQLError(..))

-- $setup
-- >>> :set -XDataKinds -XTypeOperators

-- | Argument operator. Can only be used with 'Field'.
--
-- Say we have a @Company@ object that has a field that shows whether
-- someone is an employee, e.g.
--
-- @
--   type Company {
--     hasEmployee(employeeName: String!): String!
--   }
-- @
--
-- Then we might represent that as:
--
-- >>> type Company = Object "Company" '[] '[Argument "employeeName" Text :> Field "hasEmployee" Bool]
--
-- For multiple arguments, simply chain them together with ':>', ending
-- finally with 'Field'. e.g.
--
-- @
--   Argument "foo" String :> Argument "bar" Int :> Field "qux" Int
-- @
data a :> b = a :> b
infixr 8 :>


data Object (name :: Symbol) (interfaces :: [Type]) (fields :: [Type])
data Enum (name :: Symbol) (values :: Type)
data Union (name :: Symbol) (types :: [Type])
data List (elemType :: Type)

-- TODO(tom): AFACIT We can't constrain "fields" to e.g. have at least
-- one field in it - is this a problem?
data Interface (name :: Symbol) (fields :: [Type])
data Field (name :: Symbol) (fieldType :: Type)
data Argument (name :: Symbol) (argType :: Type)


-- | The type-level schema was somehow invalid.
data SchemaError
  = NameError NameError
  | EmptyFieldList
  | EmptyUnion
  deriving (Eq, Show)

instance GraphQLError SchemaError where
  formatError (NameError err) = formatError err
  formatError EmptyFieldList = "Empty field list in type definition"
  formatError EmptyUnion = "Empty object list in union"

nameFromSymbol :: forall (n :: Symbol). KnownSymbol n => Either SchemaError Name
nameFromSymbol = first NameError (Name.nameFromSymbol @n)

-- | Specify a default value for a type in a GraphQL schema.
--
-- GraphQL schema can have default values in certain places. For example,
-- arguments to fields can have default values. Because we cannot lift
-- arbitrary values to the type level, we need some way of getting at those
-- values. This typeclass provides the means.
--
-- To specify a default, implement this typeclass.
--
-- The default implementation is to say that there *is* no default for this
-- type.
class Defaultable a where
  -- | defaultFor returns the value to be used when no value has been given.
  defaultFor :: Name -> Maybe a
  defaultFor _ = empty

instance Defaultable Int32

instance Defaultable Double

instance Defaultable Bool

instance Defaultable Text

instance Defaultable (Maybe a) where
  -- | The default for @Maybe a@ is @Nothing@.
  defaultFor _ = pure Nothing


cons :: a -> [a] -> [a]
cons = (:)

singleton :: a -> NonEmpty a
singleton x = x :| []

-- Transform into a Schema definition
class HasObjectDefinition a where
  -- Todo rename to getObjectTypeDefinition
  getDefinition :: Either SchemaError Schema.ObjectTypeDefinition

class HasFieldDefinition a where
  getFieldDefinition :: Either SchemaError Schema.FieldDefinition


-- Fields
class HasFieldDefinitions a where
  getFieldDefinitions :: Either SchemaError (NonEmpty Schema.FieldDefinition)

instance forall a as. (HasFieldDefinition a, HasFieldDefinitions as) => HasFieldDefinitions (a:as) where
  getFieldDefinitions =
    case getFieldDefinitions @as of
      Left EmptyFieldList -> singleton <$> getFieldDefinition @a
      Left err -> Left err
      Right fields -> NonEmpty.cons <$> getFieldDefinition @a <*> pure fields

instance HasFieldDefinitions '[] where
  getFieldDefinitions = Left EmptyFieldList


-- object types from union type lists, e.g. for
-- Union "Horse" '[Leg, Head, Tail]
--               ^^^^^^^^^^^^^^^^^^ this part
class HasUnionTypeObjectTypeDefinitions a where
  getUnionTypeObjectTypeDefinitions :: Either SchemaError (NonEmpty Schema.ObjectTypeDefinition)

instance forall a as. (HasObjectDefinition a, HasUnionTypeObjectTypeDefinitions as) => HasUnionTypeObjectTypeDefinitions (a:as) where
  getUnionTypeObjectTypeDefinitions =
    case getUnionTypeObjectTypeDefinitions @as of
      Left EmptyUnion -> singleton <$> getDefinition @a
      Left err -> Left err
      Right objects -> NonEmpty.cons <$> getDefinition @a <*> pure objects

instance HasUnionTypeObjectTypeDefinitions '[] where
  getUnionTypeObjectTypeDefinitions = Left EmptyUnion

-- Interfaces
class HasInterfaceDefinitions a where
  getInterfaceDefinitions :: Either SchemaError Schema.Interfaces

instance forall a as. (HasInterfaceDefinition a, HasInterfaceDefinitions as) => HasInterfaceDefinitions (a:as) where
  getInterfaceDefinitions = cons <$> getInterfaceDefinition @a <*> getInterfaceDefinitions @as

instance HasInterfaceDefinitions '[] where
  getInterfaceDefinitions = pure []

class HasInterfaceDefinition a where
  getInterfaceDefinition :: Either SchemaError Schema.InterfaceTypeDefinition

instance forall ks fields. (KnownSymbol ks, HasFieldDefinitions fields) => HasInterfaceDefinition (Interface ks fields) where
  getInterfaceDefinition =
    let name = nameFromSymbol @ks
        fields = getFieldDefinitions @fields
    in Schema.InterfaceTypeDefinition <$> name <*> fields

-- Give users some help if they don't terminate Arguments with a Field:
-- NB the "redundant constraints" warning is a GHC bug: https://ghc.haskell.org/trac/ghc/ticket/11099
instance forall ks t. TypeError ('Text ":> Arguments must end with a Field") =>
         HasFieldDefinition (Argument ks t) where
  getFieldDefinition = panic ":> Arugments must end with a Field. This should not happen, but rather we'll get a compile-time error instead."

instance forall ks is ts. (KnownSymbol ks, HasInterfaceDefinitions is, HasFieldDefinitions ts) => HasAnnotatedType (Object ks is ts) where
  getAnnotatedType =
    let obj = getDefinition @(Object ks is ts)
    in (Schema.TypeNamed . Schema.DefinedType . Schema.TypeDefinitionObject) <$> obj

instance forall t ks. (KnownSymbol ks, HasAnnotatedType t) => HasFieldDefinition (Field ks t) where
  getFieldDefinition =
    let name = nameFromSymbol @ks
    in Schema.FieldDefinition <$> name <*> pure [] <*> getAnnotatedType @t

class HasArgumentDefinition a where
  getArgumentDefinition :: Either SchemaError Schema.ArgumentDefinition

instance forall ks t. (KnownSymbol ks, HasAnnotatedInputType t) => HasArgumentDefinition (Argument ks t) where
  getArgumentDefinition = Schema.ArgumentDefinition <$> argName <*> argType <*> defaultValue
    where
      argName = nameFromSymbol @ks
      argType = getAnnotatedInputType @t
      defaultValue = pure Nothing

instance forall a b. (HasArgumentDefinition a, HasFieldDefinition b) => HasFieldDefinition (a :> b) where
  getFieldDefinition =
    prependArg <$> argument <*> getFieldDefinition @b
    where
      prependArg arg (Schema.FieldDefinition name argDefs at) = Schema.FieldDefinition name (arg:argDefs) at
      argument = getArgumentDefinition @a

instance forall ks is fields.
  (KnownSymbol ks, HasInterfaceDefinitions is, HasFieldDefinitions fields) =>
  HasObjectDefinition (Object ks is fields) where
  getDefinition =
    let name = nameFromSymbol @ks
        interfaces = getInterfaceDefinitions @is
        fields = getFieldDefinitions @fields
    in Schema.ObjectTypeDefinition <$> name <*> interfaces <*> fields

-- Builtin output types (annotated types)
class HasAnnotatedType a where
  -- TODO - the fact that we have to return TypeNonNull for normal
  -- types will amost certainly lead to bugs because people will
  -- forget this. Maybe we can flip the internal encoding to be
  -- non-null by default and needing explicit null-encoding (via
  -- Maybe).
  getAnnotatedType :: Either SchemaError (Schema.AnnotatedType Schema.GType)

-- | Turn a non-null type into the optional version of its own type.
dropNonNull :: Schema.AnnotatedType t -> Schema.AnnotatedType t
dropNonNull (Schema.TypeNonNull (Schema.NonNullTypeNamed t)) = Schema.TypeNamed t
dropNonNull (Schema.TypeNonNull (Schema.NonNullTypeList t)) = Schema.TypeList t
dropNonNull x@(Schema.TypeNamed _) = x
dropNonNull x@(Schema.TypeList _) = x

instance forall a. HasAnnotatedType a => HasAnnotatedType (Maybe a) where
  -- see TODO in HasAnnotatedType class
  getAnnotatedType = dropNonNull <$> getAnnotatedType @a

builtinType :: Schema.Builtin -> Either SchemaError (Schema.AnnotatedType Schema.GType)
builtinType = pure . Schema.TypeNonNull . Schema.NonNullTypeNamed . Schema.BuiltinType

-- TODO(jml): Given that AnnotatedType is parametrised, we can probably reduce
-- a great deal of duplication by making HasAnnotatedType a parametrised type
-- class.

-- TODO(jml): Be smarter and figure out how to say "all integral types" rather
-- than listing each individually.

instance HasAnnotatedType Int where
  getAnnotatedType = builtinType Schema.GInt

instance HasAnnotatedType Int32 where
  getAnnotatedType = builtinType Schema.GInt

instance HasAnnotatedType Bool where
  getAnnotatedType = builtinType Schema.GBool

instance HasAnnotatedType Text where
  getAnnotatedType = builtinType Schema.GString

instance HasAnnotatedType Double where
  getAnnotatedType = builtinType Schema.GFloat

instance HasAnnotatedType Float where
  getAnnotatedType = builtinType Schema.GFloat

instance forall t. (HasAnnotatedType t) => HasAnnotatedType (List t) where
  getAnnotatedType = Schema.TypeList . Schema.ListType <$> getAnnotatedType @t

instance forall ks enum. (KnownSymbol ks, GraphQLEnum enum) => HasAnnotatedType (Enum ks enum) where
  getAnnotatedType = do
    let name = nameFromSymbol @ks
    let enums = sequenceA (enumValues @enum) :: Either NameError [Schema.Name]
    let et = Schema.EnumTypeDefinition <$> name <*> map (map Schema.EnumValueDefinition) (first NameError enums)
    Schema.TypeNonNull . Schema.NonNullTypeNamed . Schema.DefinedType . Schema.TypeDefinitionEnum <$> et

instance forall ks as. (KnownSymbol ks, HasUnionTypeObjectTypeDefinitions as) => HasAnnotatedType (Union ks as) where
  getAnnotatedType =
    let name = nameFromSymbol @ks
        types = getUnionTypeObjectTypeDefinitions @as
    in (Schema.TypeNamed . Schema.DefinedType . Schema.TypeDefinitionUnion) <$> (Schema.UnionTypeDefinition <$> name <*> types)

-- Help users with better type errors
instance TypeError ('Text "Cannot encode Integer because it has arbitrary size but the JSON encoding is a number") =>
         HasAnnotatedType Integer where
  getAnnotatedType = panic "Cannot encode Integer into JSON due to its arbitrary size. Should get a compile-time error instead of this."


-- Builtin input types
class HasAnnotatedInputType a where
  -- See TODO comment in "HasAnnotatedType" class for nullability.
  getAnnotatedInputType :: Either SchemaError (Schema.AnnotatedType Schema.InputType)
  default getAnnotatedInputType :: (Generic a, GenericAnnotatedInputType (Rep a)) => Either SchemaError (Schema.AnnotatedType Schema.InputType)
  getAnnotatedInputType = genericGetAnnotatedInputType @(Rep a)

instance forall a. HasAnnotatedInputType a => HasAnnotatedInputType (Maybe a) where
  getAnnotatedInputType = dropNonNull <$> getAnnotatedInputType @a

builtinInputType :: Schema.Builtin -> Either SchemaError (Schema.AnnotatedType Schema.InputType)
builtinInputType = pure . Schema.TypeNonNull . Schema.NonNullTypeNamed . Schema.BuiltinInputType

instance HasAnnotatedInputType Int where
  getAnnotatedInputType = builtinInputType Schema.GInt

instance HasAnnotatedInputType Int32 where
  getAnnotatedInputType = builtinInputType Schema.GInt

instance HasAnnotatedInputType Bool where
  getAnnotatedInputType = builtinInputType Schema.GBool

instance HasAnnotatedInputType Text where
  getAnnotatedInputType = builtinInputType Schema.GString

instance HasAnnotatedInputType Double where
  getAnnotatedInputType = builtinInputType Schema.GFloat

instance HasAnnotatedInputType Float where
  getAnnotatedInputType = builtinInputType Schema.GFloat

instance forall t. (HasAnnotatedInputType t) => HasAnnotatedInputType (List t) where
  getAnnotatedInputType = Schema.TypeList . Schema.ListType <$> getAnnotatedInputType @t

instance forall ks enum. (KnownSymbol ks, GraphQLEnum enum) => HasAnnotatedInputType (Enum ks enum) where
  getAnnotatedInputType = do
    let name = nameFromSymbol @ks
        enums = sequenceA (enumValues @enum) :: Either NameError [Schema.Name]
    let et = Schema.EnumTypeDefinition <$> name <*> map (map Schema.EnumValueDefinition) (first NameError enums)
    Schema.TypeNonNull . Schema.NonNullTypeNamed . Schema.DefinedInputType . Schema.InputTypeDefinitionEnum <$> et


-- Generic getAnnotatedInputType function
class GenericAnnotatedInputType (f :: Type -> Type) where
  genericGetAnnotatedInputType :: Either SchemaError (Schema.AnnotatedType Schema.InputType)

class GenericInputObjectFieldDefinitions (f :: Type -> Type) where
  genericGetInputObjectFieldDefinitions :: Either SchemaError (NonEmpty Schema.InputObjectFieldDefinition)

instance forall dataName consName records m p f.
  ( KnownSymbol dataName
  , KnownSymbol consName
  , GenericInputObjectFieldDefinitions records
  ) => GenericAnnotatedInputType (D1 ('MetaData dataName m p 'False)
                                  (C1 ('MetaCons consName f 'True) records
                                  )) where
  genericGetAnnotatedInputType = do
    name <- nameFromSymbol @dataName
    map ( Schema.TypeNonNull
          . Schema.NonNullTypeNamed
          . Schema.DefinedInputType
          . Schema.InputTypeDefinitionObject
          . Schema.InputObjectTypeDefinition name
        ) (genericGetInputObjectFieldDefinitions @records)

instance forall l r.
  ( GenericInputObjectFieldDefinitions l
  , GenericInputObjectFieldDefinitions r
  ) => GenericInputObjectFieldDefinitions (l :*: r) where
  genericGetInputObjectFieldDefinitions = do
    l <- genericGetInputObjectFieldDefinitions @l
    r <- genericGetInputObjectFieldDefinitions @r
    pure (l <> r)

instance forall wrappedType fieldName u s l.
  ( KnownSymbol fieldName
  , HasAnnotatedInputType wrappedType
  ) => GenericInputObjectFieldDefinitions (S1 ('MetaSel ('Just fieldName) u s l) (Rec0 wrappedType)) where
  genericGetInputObjectFieldDefinitions = do
    name <- nameFromSymbol @fieldName
    annotatedInputType <- getAnnotatedInputType @wrappedType
    let l = Schema.InputObjectFieldDefinition name annotatedInputType Nothing
    pure (l :| [])
