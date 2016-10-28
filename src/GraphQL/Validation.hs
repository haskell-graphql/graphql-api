module GraphQL.Validation
  ( ValidationError(..)
  , ValidDocument
  , validate
  , getErrors
  ) where

import Protolude

import qualified Data.GraphQL.AST as AST

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

-- XXX: Would Data.Validation make this better / simpler?

-- XXX: Lenses for the AST might be nice. If I knew lenses.

-- XXX: Beginning to think that we might as well have Arbitrary instances for
-- the AST and determine properties.

getErrors :: AST.Document -> [ValidationError]
getErrors doc = duplicateOperations
  where
    duplicateOperations = DuplicateOperation <$> findDuplicates nodeNames
    nodeNames = [ getNodeName . getNode $ op | AST.DefinitionOperation op <- getDefinitions doc ]


getDefinitions :: AST.Document -> [AST.Definition]
getDefinitions (AST.Document defns) = defns

getNode :: AST.OperationDefinition -> AST.Node
getNode (AST.Query n) = n
getNode (AST.Mutation n) = n

-- XXX: Lots of things have names. Maybe we should define a typeclass for
-- getting the name?
getNodeName :: AST.Node -> AST.Name
getNodeName (AST.Node name _ _ _) = name


-- XXX: Untested, really should have tests.
findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = findDups (sort xs)
  where
    findDups [] = []
    findDups [_] = []
    findDups (x:ys@(y:zs))
      | x == y = x:findDups (dropWhile (== x) zs)
      | otherwise = findDups ys
