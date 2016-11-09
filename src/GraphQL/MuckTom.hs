{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeFamilyDependencies #-}
{-# LANGUAGE GADTs, AllowAmbiguousTypes, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeApplications, TypeInType #-}
{-# LANGUAGE OverloadedLabels, MagicHash #-}

module GraphQL.MuckTom where

-- TODO (probably incomplete, the spec is large)
-- * input objects - I'm not super clear from the spec on how
--   they differ from normal objects.
-- * "extend type X" is used in examples in the spec but it's not
--   explained anywhere?
-- * Directives (https://facebook.github.io/graphql/#sec-Type-System.Directives)
-- * Enforce non-empty lists (might only be doable via value-level validation)

import GraphQL.Schema hiding (Type)
import qualified GraphQL.Schema (Type)
import Protolude hiding (Enum)
import qualified Prelude (String, Show(show))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.OverloadedLabels (IsLabel(..))
import GHC.Exts  (Proxy#, proxy#)

import Bookkeeper ((:=>), Book, emptyBook, (=:))
import qualified Bookkeeper
import qualified Bookkeeper.Internal as Bookkeeper
import Data.Type.Map (Mapping)
import qualified Data.Type.Map as Map
import qualified GraphQL.Value as Value
import qualified Data.Map as M

-- | Argument operator.
data a :> b = a :> b
infixr 8 :>

  -- | Object result operator.
data a :<> b = a :<> b
infixr 8 :<>


data Object (name :: Symbol) (interfaces :: [Type]) (fields :: [Type])
data Enum (name :: Symbol) (values :: [Symbol])
data Union (name :: Symbol) (types :: [Type])
data List (elemType :: Type)

-- TODO(tom): AFACIT We can't constrain "fields" to e.g. have at least
-- one field in it - is this a problem?
data Interface (name :: Symbol) (fields :: [Type])
data Field (name :: Symbol) (fieldType :: Type)
data Argument (name :: Symbol) (argType :: Type)


-- Can't set the value for default arguments via types, but can
-- distinguish to force users to provide a default argument somewhere
-- in their function (using Maybe? ore some new type like
-- https://hackage.haskell.org/package/optional-args-1.0.1)
data DefaultArgument (name :: Symbol) (argType :: Type)


-- Transform into a Schema definition
class HasObjectDefinition a where
  -- Todo rename to getObjectTypeDefinition
  getDefinition :: ObjectTypeDefinition

class HasFieldDefinition a where
  getFieldDefinition :: FieldDefinition


-- Fields
class HasFieldDefinitions a where
  getFieldDefinitions :: [FieldDefinition]

instance forall a as. (HasFieldDefinition a, HasFieldDefinitions as) => HasFieldDefinitions (a:as) where
  getFieldDefinitions = (getFieldDefinition @a):(getFieldDefinitions @as)

instance HasFieldDefinitions '[] where
  getFieldDefinitions = []

-- symbols
class GetSymbolList a where
  getSymbolList :: [Prelude.String]

instance forall a as. (KnownSymbol a, GetSymbolList as) => GetSymbolList (a:as) where
  getSymbolList = (symbolVal (Proxy :: Proxy a)):(getSymbolList @as)

instance GetSymbolList '[] where
  getSymbolList = []


-- object types from union type lists, e.g. for
-- Union "Horse" '[Leg, Head, Tail]
--               ^^^^^^^^^^^^^^^^^^ this part
class UnionTypeObjectTypeDefinitionList a where
  getUnionTypeObjectTypeDefinitions :: [ObjectTypeDefinition]

instance forall a as. (HasObjectDefinition a, UnionTypeObjectTypeDefinitionList as) => UnionTypeObjectTypeDefinitionList (a:as) where
  getUnionTypeObjectTypeDefinitions = (getDefinition @a):(getUnionTypeObjectTypeDefinitions @as)

instance UnionTypeObjectTypeDefinitionList '[] where
  getUnionTypeObjectTypeDefinitions = []


-- Interfaces
class HasInterfaceDefinitions a where
  getInterfaceDefinitions :: Interfaces

instance forall a as. (HasInterfaceDefinition a, HasInterfaceDefinitions as) => HasInterfaceDefinitions (a:as) where
  getInterfaceDefinitions = (getInterfaceDefinition @a):(getInterfaceDefinitions @as)

instance HasInterfaceDefinitions '[] where
  getInterfaceDefinitions = []

class HasInterfaceDefinition a where
  getInterfaceDefinition :: InterfaceTypeDefinition

instance forall ks fields. (KnownSymbol ks, HasFieldDefinitions fields) => HasInterfaceDefinition (Interface ks fields) where
  getInterfaceDefinition =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        in InterfaceTypeDefinition name (NonEmptyList (getFieldDefinitions @fields))

-- Give users some help if they don't terminate Arguments with a Field:
-- NB the "redundant constraints" warning is a GHC bug: https://ghc.haskell.org/trac/ghc/ticket/11099
instance forall ks t. GHC.TypeLits.TypeError ('GHC.TypeLits.Text ":> Arguments must end with a Field") =>
         HasFieldDefinition (Argument ks t) where
  getFieldDefinition = undefined

instance forall ks is ts. (KnownSymbol ks, HasInterfaceDefinitions is, HasFieldDefinitions ts) => HasAnnotatedType (Object ks is ts) where
  getAnnotatedType =
    let obj = getDefinition @(Object ks is ts)
    in TypeNamed (DefinedType (TypeDefinitionObject obj))

instance forall t ks. (KnownSymbol ks, HasAnnotatedType t) => HasFieldDefinition (Field ks t) where
  getFieldDefinition =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
    in FieldDefinition name [] (getAnnotatedType @t)


instance forall ks t b. (KnownSymbol ks, HasAnnotatedInputType t, HasFieldDefinition b) => HasFieldDefinition ((Argument ks t) :> b) where
  getFieldDefinition =
    let (FieldDefinition name argDefs at) = getFieldDefinition @b
        argName = Name (toS (symbolVal (Proxy :: Proxy ks)))
        arg = ArgumentDefinition argName (getAnnotatedInputType @t) Nothing
    in (FieldDefinition name (arg:argDefs) at)


instance forall ks is fields.
  (KnownSymbol ks, HasInterfaceDefinitions is, HasFieldDefinitions fields) =>
  HasObjectDefinition (Object ks is fields) where
  getDefinition =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
    in ObjectTypeDefinition name (getInterfaceDefinitions @is) (NonEmptyList (getFieldDefinitions @fields))

-- Builtin output types (annotated types)
class HasAnnotatedType a where
  -- TODO - the fact that we have to return TypeNonNull for normal
  -- types will amost certainly lead to bugs because people will
  -- forget this. Maybe we can flip the internal encoding to be
  -- non-null by default and needing explicit null-encoding (via
  -- Maybe).
  getAnnotatedType :: AnnotatedType GraphQL.Schema.Type

instance forall a. HasAnnotatedType a => HasAnnotatedType (Maybe a) where
  -- see TODO in HasAnnotatedType class
  getAnnotatedType =
    let TypeNonNull (NonNullTypeNamed t) = getAnnotatedType @a
    in TypeNamed t

instance HasAnnotatedType Int where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GInt

instance HasAnnotatedType Bool where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GBool

instance HasAnnotatedType Text where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GString

instance HasAnnotatedType Double where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GFloat

instance HasAnnotatedType Float where
  getAnnotatedType = (TypeNonNull . NonNullTypeNamed . BuiltinType) GFloat

instance forall t. (HasAnnotatedType t) => HasAnnotatedType (List t) where
  getAnnotatedType = TypeList (ListType (getAnnotatedType @t))

instance forall ks sl. (KnownSymbol ks, GetSymbolList sl) => HasAnnotatedType (Enum ks sl) where
  getAnnotatedType =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        et = EnumTypeDefinition name (map (EnumValueDefinition . Name . toS) (getSymbolList @sl))
    in TypeNonNull (NonNullTypeNamed (DefinedType (TypeDefinitionEnum et)))

instance forall ks as. (KnownSymbol ks, UnionTypeObjectTypeDefinitionList as) => HasAnnotatedType (Union ks as) where
  getAnnotatedType =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        types = NonEmptyList (getUnionTypeObjectTypeDefinitions @as)
    in TypeNamed (DefinedType (TypeDefinitionUnion (UnionTypeDefinition name types)))

-- Help users with better type errors
instance GHC.TypeLits.TypeError ('GHC.TypeLits.Text "Cannot encode Integer because it has arbitrary size but the JSON encoding is a number") =>
         HasAnnotatedType Integer where
  getAnnotatedType = undefined


-- Builtin input types
class HasAnnotatedInputType a where
  -- See TODO comment in "HasAnnotatedType" class for nullability.
  getAnnotatedInputType :: AnnotatedType InputType

instance forall a. HasAnnotatedInputType a => HasAnnotatedInputType (Maybe a) where
  getAnnotatedInputType =
    let TypeNonNull (NonNullTypeNamed t) = getAnnotatedInputType @a
    in TypeNamed t

instance HasAnnotatedInputType Int where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GInt

instance HasAnnotatedInputType Bool where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GBool

instance HasAnnotatedInputType Text where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GString

instance HasAnnotatedInputType Double where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GFloat

instance HasAnnotatedInputType Float where
  getAnnotatedInputType = (TypeNonNull . NonNullTypeNamed . BuiltinInputType) GFloat

instance forall t. (HasAnnotatedInputType t) => HasAnnotatedInputType (List t) where
  getAnnotatedInputType = TypeList (ListType (getAnnotatedInputType @t))

instance forall ks sl. (KnownSymbol ks, GetSymbolList sl) => HasAnnotatedInputType (Enum ks sl) where
  getAnnotatedInputType =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        et = EnumTypeDefinition name (map (EnumValueDefinition . Name . toS) (getSymbolList @sl))
    in TypeNonNull (NonNullTypeNamed (DefinedInputType (InputTypeDefinitionEnum et)))


-- How would we build handlers?
-- constraints
-- * impossible to make mistakes
--   - that excludes callback because we could forget to callback
--   - unless we can figure out a way to return a unique value from callback?
-- * no super-crazy type magic
--   - that excludes vinyl
-- * nested tuples make it hard to write single-tuple entries (Identity or Only like in postgresql-simple could work?)
--   - nested tuples need unpacking to be usable
--   - could do sth isomorphic to nested tuples (e.g. with type operators like :<>)

-- On returning
-- A: pure (10 :<> 20)
-- B: pure 10 :<> pure 20
--
-- Only solution B allows us to only run actions when the user
-- requested them. It's more to write though!


type family FieldArgument a :: Type
type instance FieldArgument Int = Int
type instance FieldArgument Int32 = Int32
type instance FieldArgument Text = Text
type instance FieldArgument Float = Float


type family FieldReturnType (m :: Type -> Type) (a :: Type) :: Type
type instance FieldReturnType m (Field ks next) = (FieldReturnType m next)
type instance FieldReturnType m (Argument a0 t :> next) = (FieldArgument t) -> FieldReturnType m next
type instance FieldReturnType m (Object ks interfaces fields) = HandlerType m (Object ks interfaces fields)

type instance FieldReturnType m Int = m Int
type instance FieldReturnType m Text = m Text
type instance FieldReturnType m Float = m Float
-- TODO: If we don't have a type instance for a type we get:
-- "Couldn't match type ‘FieldReturnType IO Int32’ with ‘f0 a0’"
-- Can we make that a nicer error message? Doesn't seem trivial ...
type instance FieldReturnType m Int32 = m Int32


type family FieldName (a :: Type) :: Symbol
type instance FieldName (Field ks t) = ks
type instance FieldName (Argument a0 a1 :> a) = FieldName a

type family ObjectReturnType (m :: Type -> Type) (a :: [Type]) :: [Mapping Symbol Type]
type instance ObjectReturnType m (a:as) = ((FieldName a) :=> (FieldReturnType m a)):(ObjectReturnType m as)
type instance ObjectReturnType m '[] = '[]

type family HandlerType (m :: Type -> Type) (a :: Type) :: Type
type instance HandlerType m (Object ks interfaces fields) = Bookkeeper.Book' (ObjectReturnType m fields)

-- simple text placeholder query for now
type Query = [Text]

class HasGraph a where
  buildResolver :: HandlerType IO a -> Query -> IO (M.Map Text (IO Value.Value))


class RunFields a where
  runFields :: HandlerType IO a -> M.Map Text (IO Value.Value)

instance forall ks x interfaces fields v xs.
  ( KnownSymbol ks
--  , HandlerType IO (Object x interfaces fields) ~ Bookkeeper.Book' ((ks :=> v) ': xs)
  ) => RunFields (Object x interfaces fields) where
  runFields (Bookkeeper.Book (Map.Ext key ioValue rest)) = traceShow key M.empty


-- todo: arguments. The interpreter
instance forall ks interfaces fields x.
  ( KnownSymbol ks
  ) => HasGraph (Object ks interfaces fields) where
  buildResolver handler = \query -> do
    let r = handler -- TODO handler should run in MonadIO so it can run do e.g. dbQueries
    let rf = runFields @(Object ks interfaces fields) r
    pure rf


type T = Object "T" '[] '[Field "z" Int32, Field "t" Int32]
tHandler :: HandlerType IO T
tHandler = emptyBook &  #t =: pure 10 & #z =: pure 10

type Calculator = Object "Calculator" '[]
  '[ Argument "a" Int :> Argument "b" Int :> Field "add" Int
   , Argument "a" Float :> Field "log" Float
   ]

type API = Object "API" '[] '[Field "calc" Calculator]

calculatorHandler :: HandlerType IO Calculator
calculatorHandler =
  emptyBook & #add =: add' & #log =: log'
  where
    add' a b = pure (a + b)
    log' a = pure (log a)

api :: HandlerType IO API
api = emptyBook & #calc =: calculatorHandler
