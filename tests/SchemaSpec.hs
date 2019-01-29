{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SchemaSpec (spec) where

import Protolude hiding (Down, Enum)

import Test.Hspec

import GraphQL.API
  ( Field
  , Enum
  , List
  , getAnnotatedInputType
  , getDefinition
  )
import qualified GraphQL.Internal.Syntax.AST as AST
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
  , InterfaceTypeDefinition(..)
  , AnnotatedType(..)
  , ListType(..)
  , UnionTypeDefinition(..)
  , GType(..)
  , TypeDefinition(..)
  , InputTypeDefinition(..)
  , InputObjectTypeDefinition(..)
  , InputObjectFieldDefinition(..)
  , ScalarTypeDefinition(..)
  , AnnotatedType(..)
  , NonNullType(..)
  , Builtin(..)
  , InputType(..)
  , getInputTypeDefinition
  , builtinFromName
  , astAnnotationToSchemaAnnotation
  )
import ExampleSchema

spec :: Spec
spec = describe "Type" $ do
  describe "Field" $
    it "encodes correctly" $ do
    getFieldDefinition @(Field "hello" Int) `shouldBe` Right (FieldDefinition "hello" [] (TypeNonNull (NonNullTypeNamed (BuiltinType GInt))))
  describe "Interface" $
    it "encodes correctly" $ do
    getInterfaceDefinition @Sentient `shouldBe`
      Right (InterfaceTypeDefinition
        "Sentient"
        (FieldDefinition "name" [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString))) :| []))
  describe "full example" $
    it "encodes correctly" $ do
    getDefinition @Human `shouldBe`
      Right (ObjectTypeDefinition "Human"
        [ InterfaceTypeDefinition "Sentient" (
            FieldDefinition "name" [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString))) :| [])
        ]
        (FieldDefinition "name" [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString))) :| []))
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
        <$> sequence (getDefinition @Cat :| [getDefinition @Dog])
  describe "List" $
    it "encodes correctly" $ do
    getAnnotatedType @(List Int) `shouldBe` Right (TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinType GInt)))))
    getAnnotatedInputType @(List Int) `shouldBe` Right (TypeList (ListType (TypeNonNull (NonNullTypeNamed (BuiltinInputType GInt)))))
  describe "TypeDefinition accepted as InputTypes" $
    it "Enum/InputObject/Scalar" $ do
    getInputTypeDefinition (TypeDefinitionEnum (EnumTypeDefinition "DogCommand"
     [ EnumValueDefinition "Sit"
     , EnumValueDefinition "Down"
     , EnumValueDefinition "Heel"
     ])) `shouldBe` Just (InputTypeDefinitionEnum (EnumTypeDefinition "DogCommand"
     [ EnumValueDefinition "Sit"
     , EnumValueDefinition "Down"
     , EnumValueDefinition "Heel"
     ]))
    getInputTypeDefinition (TypeDefinitionInputObject (InputObjectTypeDefinition  "Human"
     (InputObjectFieldDefinition "name" (TypeNonNull (NonNullTypeNamed (BuiltinInputType GString))) Nothing :| [])
     )) `shouldBe` Just (InputTypeDefinitionObject (InputObjectTypeDefinition "Human"
     (InputObjectFieldDefinition "name" (TypeNonNull (NonNullTypeNamed (BuiltinInputType GString))) Nothing :| [])
     ))
    getInputTypeDefinition (TypeDefinitionScalar (ScalarTypeDefinition  "Human")) `shouldBe` Just (InputTypeDefinitionScalar (ScalarTypeDefinition  "Human"))
  describe "TypeDefinition refused as InputTypes" $
    -- todo: add all the others (union type, ..?)
    it "Object" $ do
    getInputTypeDefinition (TypeDefinitionObject (ObjectTypeDefinition "Human" []
        (FieldDefinition "name" [] (TypeNonNull (NonNullTypeNamed (BuiltinType GString))) :| []))) `shouldBe` Nothing
  describe "Builtin types from name" $
    it "Int/Bool/String/Float/ID" $ do
    builtinFromName "Int" `shouldBe` Just GInt
    builtinFromName "Boolean" `shouldBe` Just GBool
    builtinFromName "String" `shouldBe` Just GString
    builtinFromName "Float" `shouldBe` Just GFloat
    builtinFromName "ID" `shouldBe` Just GID
    builtinFromName "RANDOMSTRING" `shouldBe` Nothing
  describe "Annotations from AST" $
    it "annotation like [[ScalarType!]]!" $ do
    let typeDefinitionScalar = (TypeDefinitionScalar (ScalarTypeDefinition "ScalarType"))
    astAnnotationToSchemaAnnotation (
      AST.TypeNonNull (
        AST.NonNullTypeList (
          AST.ListType (
            AST.TypeList (
              AST.ListType (
                AST.TypeNonNull (
                  AST.NonNullTypeNamed (AST.NamedType "ScalarType")
      ))))))) typeDefinitionScalar `shouldBe` (
        TypeNonNull (
          NonNullTypeList (
            ListType (
              TypeList (
                ListType (
                  TypeNonNull (
                    NonNullTypeNamed typeDefinitionScalar
        )))))))
