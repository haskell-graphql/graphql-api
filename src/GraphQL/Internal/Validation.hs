module GraphQL.Internal.Validation
  ( ValidationError(..)
  , ValidDocument
  , validate
  , getErrors
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

newtype ValidDocument = Valid AST.Document deriving (Eq, Show)

validate :: Alternative m => AST.Document -> m ValidDocument
validate = pure . Valid

data ValidationError
  = DuplicateOperation AST.Name
  deriving (Eq, Show)


getErrors :: AST.Document -> [ValidationError]
getErrors doc = duplicateOperations
  where
    duplicateOperations = DuplicateOperation <$> findDuplicates nodeNames
    nodeNames = [ AST.getNodeName . AST.getNode $ op | AST.DefinitionOperation op <- AST.getDefinitions doc ]


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
