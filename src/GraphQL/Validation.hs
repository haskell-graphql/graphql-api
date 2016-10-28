module GraphQL.Validation (ValidDocument, validate) where

import Protolude

import qualified Data.GraphQL.AST as AST

newtype ValidDocument = Valid AST.Document deriving (Eq, Show)

validate :: Alternative m => AST.Document -> m ValidDocument
validate = pure . Valid
