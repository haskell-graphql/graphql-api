{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SchemaTests (tests) where

import Protolude hiding (Down, Enum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import GraphQL.API
  ( Field
  , Enum
  , List
  , getAnnotatedInputType
  , getDefinition
  )
import GraphQL.Internal.API
  ( getAnnotatedType
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
  , GType(..)
  , TypeDefinition(..)
  , NonNullType(..)
  , Builtin(..)
  , InputType(..)
  )
import ExampleSchema

tests :: IO TestTree
tests = testSpec "Type" $ do
  describe "Field" $
    it "encodes correctly" $ do
    getFieldDefinition @(Field "hello" Int) `shouldBe` Right (FieldDefinition "hello" [] (TypeNonNull (NonNullTypeNamed (BuiltinType GInt))))
  describe "Interface" $
    it "encodes correctly" $ do
    getInterfaceDefinition @Sentient `shouldBe`
      Right (InterfaceTypeDefinition
        "Sentient"
        (NonEmptyList [FieldDefinition "name" [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))]))
  describe "full example" $
    it "encodes correctly" $ do
    getDefinition @Human `shouldBe`
      Right (ObjectTypeDefinition "Human"
        [ InterfaceTypeDefinition "Sentient" (
            NonEmptyList [FieldDefinition "name" [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))])
        ]
        (NonEmptyList [FieldDefinition "name" [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString)))]))
  describe "output Enum" $
    it "encodes correctly" $ do
    getAnnotatedType @(Enum "DogCommand" DogCommand) `shouldBe`
       Right (TypeNonNull (NonNullTypeNamed (DefinedType (TypeDefinitionEnum (EnumTypeDefinition "DogCommand"
         [ EnumValueDefinition "Sit"
         , EnumValueDefinition "Down"
         , EnumValueDefinition "Heel"
         ])))))
  describe "Union type" $
    it "encodes correctly" $ do
    getAnnotatedType @CatOrDog `shouldBe`
      TypeNamed . DefinedType . TypeDefinitionUnion . UnionTypeDefinition "CatOrDog"
        . NonEmptyList <$> sequence [ getDefinition @Cat
                                    , getDefinition @Dog
                                    ]
  describe "List" $
    it "encodes correctly" $ do
    getAnnotatedType @(List Int) `shouldBe` Right (TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinType GInt)))))
    getAnnotatedInputType @(List Int) `shouldBe` Right (TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinInputType GInt)))))
