module GraphQL.Internal.Validation
  ( ValidationError(..)
  , QueryDocument
  , validate
  , getErrors
  , getOperationName
  -- * Exported for testing
  , findDuplicates
  ) where

import Protolude

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.AST (Name)

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

data QueryDocument
  -- | The query document contains a single anonymous operation.
  = LoneAnonymousOperation AST.SelectionSet [AST.FragmentDefinition]
  -- | The query document contains multiple uniquely-named operations.
  | MultipleOperations (Map Name AST.OperationDefinition) [AST.FragmentDefinition]
  deriving (Eq, Show)

validate :: AST.QueryDocument -> Either (NonEmpty ValidationError) QueryDocument
validate (AST.QueryDocument defns) =
  let
    (operations, fragments) = splitBy splitDefns defns
    (anonymous, named) = splitBy splitOps operations
  in
    case (anonymous, named) of
      ([], ops) ->
        case makeMap ops of
          Left dups -> Left (map DuplicateOperation dups)
          Right ops' -> Right (MultipleOperations ops' fragments)
      ([x], []) -> Right (LoneAnonymousOperation x fragments)
      _ -> Left (singleton (MixedAnonymousOperations (length anonymous) (map fst named)))

  where
    splitBy :: (a -> Either b c) -> [a] -> ([b], [c])
    splitBy f xs = partitionEithers (map f xs)

    splitDefns (AST.DefinitionOperation op) = Left op
    splitDefns (AST.DefinitionFragment frag) = Right frag

    splitOps (AST.AnonymousQuery ss) = Left ss
    splitOps q@(AST.Query (AST.Node name _ _ _)) = Right (name, q)
    splitOps m@(AST.Mutation (AST.Node name _ _ _)) = Right (name, m)

-- TODO: Might be nice to have something that goes from a validated document
-- back to the AST. This would be especially useful for encoding, so we could
-- debug by looking at GraphQL rather than data types.

-- | Errors arising from validating a document.
data ValidationError
  -- | 'DuplicateOperation' means there was more than one operation defined
  -- with the given name.
  --
  -- https://facebook.github.io/graphql/#sec-Operation-Name-Uniqueness
  = DuplicateOperation AST.Name
  -- | 'MixedAnonymousOperations' means there was more than one operation
  -- defined in a document with an anonymous operation.
  --
  -- https://facebook.github.io/graphql/#sec-Lone-Anonymous-Operation
  | MixedAnonymousOperations Int [AST.Name]
  deriving (Eq, Show)

-- | Identify all of the validation errors in @doc@.
--
-- An empty list means no errors.
--
-- https://facebook.github.io/graphql/#sec-Validation
getErrors :: AST.QueryDocument -> [ValidationError]
getErrors doc =
  case validate doc of
    Left errors -> NonEmpty.toList errors
    Right _ -> []

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

-- | Create a map from a list of key-value pairs.
--
-- Returns a list of duplicates on 'Left' if there are duplicates.
makeMap :: Ord key => [(key, value)] -> Either (NonEmpty key) (Map key value)
makeMap entries =
  case NonEmpty.nonEmpty (findDuplicates (map fst entries)) of
    Nothing -> Right (Map.fromList entries)
    Just dups -> Left dups

-- XXX: Copied from Execution
-- | Make a non-empty list. This is just an alias for the symbolic constructor.
singleton :: a -> NonEmpty a
singleton x = x :| []
