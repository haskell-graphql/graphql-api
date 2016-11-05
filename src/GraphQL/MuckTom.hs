{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, AllowAmbiguousTypes, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeApplications, TypeInType #-}
module GraphQL.MuckTom where


import GraphQL.Schema hiding (Type)
import qualified GraphQL.Schema (Type)
import Protolude hiding (Enum)
import qualified Prelude (String)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified GHC.TypeLits (TypeError, ErrorMessage(..))

-- | Argument operator.
data a :> b = a :> b
infixr 8 :>

-- | Uniotn type
data a :<|> b = a :<|> b
infixr 8 :<|>


data Object (name :: Symbol) (interfaces :: [Type]) (fields :: [Type])
data Enum (name :: Symbol) (values :: [Symbol])

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

-- annotated types
class HasAnnotatedType a where
  getAnnotatedType :: AnnotatedType GraphQL.Schema.Type

class HasAnnotatedInputType a where
  getAnnotatedInputType :: AnnotatedType InputType

instance HasAnnotatedType Int where
  getAnnotatedType = TypeNamed (BuiltinType GInt)

instance HasAnnotatedType Bool where
  getAnnotatedType = TypeNonNull (NonNullTypeNamed (BuiltinType GInt))

-- Example for how to do type errors:
-- NB the "redundant constraints" warning is a GHC bug: https://ghc.haskell.org/trac/ghc/ticket/11099
instance GHC.TypeLits.TypeError ('GHC.TypeLits.Text "Cannot encode Integer because it has arbitrary size but the JSON encoding is a number") =>
         HasAnnotatedType Integer where
  getAnnotatedType = undefined

-- Give users some help if they don't terminate Arguments with a Field:
-- NB the "redundant constraints" warning is a GHC bug: https://ghc.haskell.org/trac/ghc/ticket/11099
instance forall ks t. GHC.TypeLits.TypeError ('GHC.TypeLits.Text ":> Arguments must end with a Field") =>
         HasFieldDefinition (Argument ks t) where
  getFieldDefinition = undefined

instance HasAnnotatedType Text where
  getAnnotatedType = TypeNamed (BuiltinType GString)

instance HasAnnotatedType User where
  getAnnotatedType =
    let fieldDefinitions = NonEmptyList
          [ getFieldDefinition @(Field "name" Text)
          , getFieldDefinition @(Field "age" Int)
          , getFieldDefinition @(Field "id" Int)
          ]
    in TypeNamed (DefinedType (TypeDefinitionObject (ObjectTypeDefinition (Name "User") [] fieldDefinitions)))

instance forall ks sl. (KnownSymbol ks, GetSymbolList sl) => HasAnnotatedInputType (Enum ks sl) where
  getAnnotatedInputType =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        et = EnumTypeDefinition name (map (EnumValueDefinition . Name . toS) (getSymbolList @sl))
    in TypeNonNull (NonNullTypeNamed (DefinedInputType (InputTypeDefinitionEnum et)))

instance HasAnnotatedInputType Int where
  getAnnotatedInputType = TypeNonNull (NonNullTypeNamed (BuiltinInputType GInt))

instance HasAnnotatedInputType Bool where
  getAnnotatedInputType = TypeNonNull (NonNullTypeNamed (BuiltinInputType GInt))

instance forall a. HasAnnotatedInputType a => HasAnnotatedInputType (Maybe a) where
  getAnnotatedInputType =
    let TypeNonNull (NonNullTypeNamed t) = getAnnotatedInputType @a
    in TypeNamed t

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


data User = User { name :: Text, age :: Int, id :: Int }
