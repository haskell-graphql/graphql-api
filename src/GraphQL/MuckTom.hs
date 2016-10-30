{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE KindSignatures, FlexibleInstances, TypeOperators, TypeApplications, TypeInType #-}
module MuckTom where

{-
:load src/GraphQL/MuckTom.hs
:set -XGADTs  -XDataKinds -XKindSignatures -XTypeApplications
-}

import GraphQL.Schema hiding (Type)
import qualified GraphQL.Schema (Type)
import Protolude
import qualified Prelude (show)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Typeable (typeRep)

data a :> b = a :> b -- arguments
infixr 8 :>

data Object (name :: Symbol) (definition :: [Type])


-- TODO(tom): AFACIT We can't constrain "fields" to e.g. have at least
-- one field in it - is this a problem?
data Interface (typeName :: Symbol) (fields :: Type)
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
  getFieldDefinitions :: [FieldDefinition]

class HasAnnotatedType a where
  getAnnotatedType :: AnnotatedType GraphQL.Schema.Type

instance HasAnnotatedType Int where
  getAnnotatedType = TypeNamed (BuiltinType GInt)

instance forall ks ts. (KnownSymbol ks, HasFieldDefinition ts) => HasAnnotatedType (Object ks ts) where
  getAnnotatedType =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
        obj = getDefinition @(Object ks ts)
    in TypeNamed (DefinedType (TypeDefinitionObject obj))

instance forall t ks ts. (KnownSymbol ks, HasFieldDefinition ts, HasAnnotatedType t) => HasFieldDefinition ((Field ks t):ts) where
  getFieldDefinitions =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
    in [FieldDefinition name [] (getAnnotatedType @t)] ++ (getFieldDefinitions @ts)


instance HasFieldDefinition '[] where
  getFieldDefinitions = []


instance forall ks t b. (KnownSymbol ks, HasAnnotatedType t, HasFieldDefinition b) => HasFieldDefinition ((Argument ks t) :> b) where
  getFieldDefinitions =
    let fd = getFieldDefinitions @b
    in fd

instance forall ks t ts. (KnownSymbol ks, HasFieldDefinition t) => HasObjectDefinition (Object ks t) where
  getDefinition =
    let name = Name (toS (symbolVal (Proxy :: Proxy ks)))
    in ObjectTypeDefinition name [] (NonEmptyList (getFieldDefinitions @t))


type TestField = Field "test-field" Int
type TestField2 = Field "test-field-2" Int
type User = Object "User" '[TestField, TestField2, Field "address" (Object "Address" '[TestField2])]

type X = Argument "test-arg" Int :> TestField

-- TODO the following should break but I don't know how to encode
-- non-empty type-lists without overlapping instances.
-- cf https://wiki.haskell.org/GHC/AdvancedOverlap
type Home = Object "Home" '[]

testDefinition :: ObjectTypeDefinition
testDefinition = getDefinition @User
