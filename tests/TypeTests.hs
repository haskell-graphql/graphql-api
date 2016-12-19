{-# LANGUAGE DataKinds, TypeOperators #-}
module TypeTests (tests) where

import Protolude hiding (Enum)

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
import GraphQL.Value (unsafeMakeName)

-- Examples taken from the spec

data DogCommandEnum = Sit | Down | Heel

instance GraphQLEnum DogCommandEnum where
  enumValues = map unsafeMakeName ["SIT", "DOWN", "HEEL"]
  enumToValue _ = undefined
  enumFromValue _ = undefined

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

type Alien = Object "Alien" '[Sentient] [Field "name" Text, Field "homePlanet" Text]

type Human = Object "Human" '[Sentient] '[Field "name" Text]

data CatCommandEnum = Jump
instance GraphQLEnum CatCommandEnum where
  enumValues = [unsafeMakeName "JUMP"]
  enumToValue = undefined
  enumFromValue = undefined

type CatCommand = Enum "CatCommand" CatCommandEnum

type Cat = Object "Cat" '[Pet]
  '[ Field "name" Text
   , Field "nickName" (Maybe Text)
   , Argument "catCommand" CatCommand :> Field "doesKnowCommand" Bool
   , Field "meowVolume" Int
   ]

type CatOrDog = Union "CatOrDog" '[Cat, Dog]
type DogOrHuman = Union "DogOrHuman" '[Dog, Human]
type HumanOrAlien = Union "HumanOrAlien" '[Human, Alien]

type QueryRoot = Object "QueryRoot" '[Field "dog" Dog]


tests :: IO TestTree
tests = testSpec "Type" $ do
  describe "Field" $
    it "encodes correctly" $ do
    getFieldDefinition @(Field "hello" Int) `shouldBe` FieldDefinition (unsafeMakeName "hello") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GInt)))
  describe "Interface" $
    it "encodes correctly" $ do
    getInterfaceDefinition @Sentient `shouldBe`
      InterfaceTypeDefinition
        (unsafeMakeName "Sentient")
        (NonEmptyList [FieldDefinition (unsafeMakeName "name") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))])
  describe "full example" $
    it "encodes correctly" $ do
    getDefinition @Human `shouldBe`
      ObjectTypeDefinition (unsafeMakeName "Human")
        [ InterfaceTypeDefinition (unsafeMakeName "Sentient") (
            NonEmptyList [FieldDefinition (unsafeMakeName "name") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))])
        ]
        (NonEmptyList [FieldDefinition (unsafeMakeName "name") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))])
  describe "output Enum" $
    it "encodes correctly" $ do
    getAnnotatedType @DogCommand `shouldBe`
       TypeNonNull (NonNullTypeNamed (DefinedType (TypeDefinitionEnum (EnumTypeDefinition (unsafeMakeName "DogCommand")
         [ EnumValueDefinition (unsafeMakeName "SIT")
         , EnumValueDefinition (unsafeMakeName "DOWN")
         , EnumValueDefinition (unsafeMakeName "HEEL")
         ]))))
  describe "Union type" $
    it "encodes correctly" $ do
    getAnnotatedType @CatOrDog `shouldBe`
      TypeNamed (DefinedType (TypeDefinitionUnion (UnionTypeDefinition (unsafeMakeName "CatOrDog")
        (NonEmptyList [ getDefinition @Cat
                      , getDefinition @Dog
                      ]
        ))))
  describe "List" $
    it "encodes correctly" $ do
    getAnnotatedType @(List Int) `shouldBe` TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinType GInt))))
    getAnnotatedInputType @(List Int) `shouldBe` TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinInputType GInt))))
