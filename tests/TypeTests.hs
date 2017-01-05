{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module TypeTests (tests) where

import Protolude hiding (Down, Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.API
  ( GraphQLEnum(..)
  , Enum
  , Object
  , Field
  , Argument
  , Interface
  , Union
  , List
  , (:>)
  , getAnnotatedType
  , getAnnotatedInputType
  , getDefinition
  , getFieldDefinition
  , getInterfaceDefinition
  )
import GraphQL.Internal.AST (unsafeMakeName)
import GraphQL.Internal.Schema
  ( EnumTypeDefinition(..)
  , EnumValueDefinition(..)
  , FieldDefinition(..)
  , ObjectTypeDefinition(..)
  , NonEmptyList(..)
  , InterfaceTypeDefinition(..)
  , AnnotatedType(..)
  , ListType(..)
  , UnionTypeDefinition(..)
  , Type(..)
  , TypeDefinition(..)
  , NonNullType(..)
  , Builtin(..)
  , InputType(..)
  )

-- Examples taken from the spec

data DogCommandEnum = Sit | Down | Heel deriving (Show, Generic)

instance GraphQLEnum DogCommandEnum

-- Alternative might be a sum type with deriving Generic and 0-arity constructors?
type DogCommand = Enum "DogCommand" DogCommandEnum


type Dog = Object "Dog" '[Pet]
  '[ Field "name" Text
   , Field "nickname" Text
   , Field "barkVolume" Int
   , Argument "dogCommand" DogCommand :> Field "doesKnowCommand" Bool
   , Argument "atOtherHomes" (Maybe Bool) :> Field "isHouseTrained" Bool
   , Field "owner" Human
   ]

type Sentient = Interface "Sentient" '[Field "name" Text]
type Pet = Interface "Pet" '[Field "name" Text]

type Human = Object "Human" '[Sentient] '[Field "name" Text]

data CatCommandEnum = Jump deriving Generic

instance GraphQLEnum CatCommandEnum

type CatCommand = Enum "CatCommand" CatCommandEnum

type Cat = Object "Cat" '[Pet]
  '[ Field "name" Text
   , Field "nickName" (Maybe Text)
   , Argument "catCommand" CatCommand :> Field "doesKnowCommand" Bool
   , Field "meowVolume" Int
   ]

type CatOrDog = Union "CatOrDog" '[Cat, Dog]


tests :: IO TestTree
tests = testSpec "Type" $ do
  describe "Field" $
    it "encodes correctly" $ do
    getFieldDefinition @(Field "hello" Int) `shouldBe` Right (FieldDefinition (unsafeMakeName "hello") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GInt))))
  describe "Interface" $
    it "encodes correctly" $ do
    getInterfaceDefinition @Sentient `shouldBe`
      Right (InterfaceTypeDefinition
        (unsafeMakeName "Sentient")
        (NonEmptyList [FieldDefinition (unsafeMakeName "name") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))]))
  describe "full example" $
    it "encodes correctly" $ do
    getDefinition @Human `shouldBe`
      Right (ObjectTypeDefinition (unsafeMakeName "Human")
        [ InterfaceTypeDefinition (unsafeMakeName "Sentient") (
            NonEmptyList [FieldDefinition (unsafeMakeName "name") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))])
        ]
        (NonEmptyList [FieldDefinition (unsafeMakeName "name") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))]))
  describe "output Enum" $
    it "encodes correctly" $ do
    getAnnotatedType @DogCommand `shouldBe`
       Right (TypeNonNull (NonNullTypeNamed (DefinedType (TypeDefinitionEnum (EnumTypeDefinition (unsafeMakeName "DogCommand")
         [ EnumValueDefinition (unsafeMakeName "Sit")
         , EnumValueDefinition (unsafeMakeName "Down")
         , EnumValueDefinition (unsafeMakeName "Heel")
         ])))))
  describe "Union type" $
    it "encodes correctly" $ do
    getAnnotatedType @CatOrDog `shouldBe`
      TypeNamed . DefinedType . TypeDefinitionUnion . UnionTypeDefinition (unsafeMakeName "CatOrDog")
        . NonEmptyList <$> sequence [ getDefinition @Cat
                                    , getDefinition @Dog
                                    ]
  describe "List" $
    it "encodes correctly" $ do
    getAnnotatedType @(List Int) `shouldBe` Right (TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinType GInt)))))
    getAnnotatedInputType @(List Int) `shouldBe` Right (TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinInputType GInt)))))
