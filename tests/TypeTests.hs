{-# LANGUAGE TypeApplications, DataKinds, TypeOperators #-}
module TypeTests where

import Protolude hiding (Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.MuckTom
import GraphQL.Schema

-- Examples taken from the spec

-- Alternative might be a sum type with deriving Generic and 0-arity constructors?
type DogCommand = Enum "DogCommand" '["SIT", "DOWN", "HEEL"]


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

type CatCommand = Enum "CatCommand" '["JUMP"]

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

-- Placeholder
cat :: GraphHandler Cat
cat object = object "test-name" (Just "test-nick-name") (\command -> True) 10

cat = "cat-name-kitty"
  :<> Just "kitty"
  :<> \command -> True
  :<> 10

human :: GraphHandler Human
human = pure
  ( Identity "my-name-is-teh" )

dog :: GraphHandler Dog
dog cb = pure $ "dog-name"
  :<> "dog nickname"
  :<> 10
  :<> \command -> True
  :<> \atOtherHomes -> True
  :<> human


typeTests :: IO TestTree
typeTests = testSpec "Type" $ do
  describe "Field" $
    it "encodes correctly" $ do
    getFieldDefinition @(Field "hello" Int) `shouldBe` (FieldDefinition (Name "hello") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GInt))))
  describe "Interface" $
    it "encodes correctly" $ do
    getInterfaceDefinition @Sentient `shouldBe` (
      InterfaceTypeDefinition
        (Name "Sentient")
        (NonEmptyList [FieldDefinition (Name "name") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))])
      )
  describe "full example" $
    it "encodes correctly" $ do
    getDefinition @Human `shouldBe` (
      ObjectTypeDefinition (Name "Human")
        [ InterfaceTypeDefinition (Name "Sentient") (
            NonEmptyList [FieldDefinition (Name "name") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))])
        ]
        (NonEmptyList [FieldDefinition (Name "name") [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))])
      )
  describe "output Enum" $
    it "encodes correctly" $ do
    getAnnotatedType @DogCommand `shouldBe` (
       TypeNonNull (NonNullTypeNamed (DefinedType (TypeDefinitionEnum (EnumTypeDefinition (Name "DogCommand")
         [ EnumValueDefinition (Name "SIT")
         , EnumValueDefinition (Name "DOWN")
         , EnumValueDefinition (Name "HEEL")
         ])))))
  describe "Union type" $
    it "encodes correctly" $ do
    getAnnotatedType @CatOrDog `shouldBe` (
      TypeNamed (DefinedType (TypeDefinitionUnion (UnionTypeDefinition (Name "CatOrDog")
        (NonEmptyList [ getDefinition @Cat
                      , getDefinition @Dog
                      ]
        )))))
  describe "List" $
    it "encodes correctly" $ do
    getAnnotatedType @(List Int) `shouldBe` (TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinType GInt)))))
    getAnnotatedInputType @(List Int) `shouldBe` (TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinInputType GInt)))))
