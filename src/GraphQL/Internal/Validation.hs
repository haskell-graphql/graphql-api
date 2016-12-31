module GraphQL.Internal.Validation
  ( ValidationError(..)
  , ValidDocument
  , validate
  , getErrors
  , getOperationName
  -- * Exported for testing
  , findDuplicates
  ) where

import Protolude

import qualified GraphQL.Internal.AST as AST

{-
enum DogCommand { SIT, DOWN, HEEL }

type Dog implements Pet {
  name: String!
  nickname: String
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand!): Boolean!
  isHousetrained(atOtherHomes: Boolean): Boolean!
  owner: Human
}

interface Sentient {
  name: String!
}

interface Pet {
  name: String!
}

type Alien implements Sentient {
  name: String!
  homePlanet: String
}

type Human implements Sentient {
  name: String!
}

enum CatCommand { JUMP }

type Cat implements Pet {
  name: String!
  nickname: String
  doesKnowCommand(catCommand: CatCommand!): Boolean!
  meowVolume: Int
}

union CatOrDog = Cat | Dog
union DogOrHuman = Dog | Human
union HumanOrAlien = Human | Alien

type QueryRoot {
  dog: Dog
}
-}

newtype ValidDocument = Valid AST.QueryDocument deriving (Eq, Show)

validate :: Alternative m => AST.QueryDocument -> m ValidDocument
validate = pure . Valid

-- | Errors arising from validating a document.
data ValidationError
  -- | 'DuplicateOperation' means there was more than one operation defined
  -- with the given name.
  --
  -- https://facebook.github.io/graphql/#sec-Operation-Name-Uniqueness
  = DuplicateOperation AST.Name
  -- | 'MultipleAnonymousOperation' means there was more than one anonymous
  -- operation defined.
  --
  -- https://facebook.github.io/graphql/#sec-Lone-Anonymous-Operation
  | MultipleAnonymousOperation Int
  deriving (Eq, Show)

-- | Identify all of the validation errors in @doc@.
--
-- An empty list means no errors.
--
-- https://facebook.github.io/graphql/#sec-Validation
getErrors :: AST.QueryDocument -> [ValidationError]
getErrors doc = duplicateOperations <> multipleAnonymousOps
  where
    duplicateOperations = map DuplicateOperation (findDuplicates (catMaybes nodeNames))
    multipleAnonymousOps =
      case length (filter (== Nothing) nodeNames) of
        0 -> mempty
        1 -> mempty
        n -> [MultipleAnonymousOperation n]
    nodeNames = [ getOperationName op | AST.DefinitionOperation op <- AST.getDefinitions doc ]

-- | 'getOperationName' returns the name of the operatioen if there is any,
-- 'Nothing' otherwise.
getOperationName :: AST.OperationDefinition -> Maybe AST.Name
getOperationName (AST.Query (AST.Node name _ _ _)) = pure name
getOperationName (AST.Mutation (AST.Node name _ _ _)) = pure name
getOperationName (AST.AnonymousQuery _) = empty

-- | Return a list of all the elements with duplicates. The list of duplicates
-- itself will not contain duplicates.
--
-- prop> \xs -> findDuplicates @Int xs == ordNub (findDuplicates @Int xs)
findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = findDups (sort xs)
  where
    findDups [] = []
    findDups [_] = []
    findDups (x:ys@(y:zs))
      | x == y = x:findDups (dropWhile (== x) zs)
      | otherwise = findDups ys
