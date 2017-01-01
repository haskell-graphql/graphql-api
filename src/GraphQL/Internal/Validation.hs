{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Transform GraphQL query documents from AST into valid structures
--
-- This corresponds roughly to the
-- [Validation](https://facebook.github.io/graphql/#sec-Validation) section of
-- the specification, except where noted.
--
-- One core difference is that this module doesn't attempt to do any
-- type-level validation, as we attempt to defer all of that to the Haskell
-- type checker.
module GraphQL.Internal.Validation
  ( ValidationError(..)
  , QueryDocument
  , validate
  , getErrors
  -- * Operating on validated documents
  , getOperation
  -- * Exported for testing
  , findDuplicates
  ) where

import Protolude

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.AST (Name)

-- | A valid query document.
--
-- Construct this using 'validate' on an 'AST.QueryDocument'.
data QueryDocument
  -- | The query document contains a single anonymous operation.
  = LoneAnonymousOperation AST.SelectionSet FragmentDefinitions
  -- | The query document contains multiple uniquely-named operations.
  | MultipleOperations (Map Name AST.OperationDefinition) FragmentDefinitions
  deriving (Eq, Show)

-- | Get an operation from a GraphQL document
--
-- Technically this is part of the "Execution" phase, but we're keeping it
-- here for now to avoid exposing constructors for valid documents.
--
-- <https://facebook.github.io/graphql/#sec-Executing-Requests>
--
-- GetOperation(document, operationName):
--
--   * If {operationName} is {null}:
--     * If {document} contains exactly one operation.
--       * Return the Operation contained in the {document}.
--     * Otherwise produce a query error requiring {operationName}.
--   * Otherwise:
--     * Let {operation} be the Operation named {operationName} in {document}.
--     * If {operation} was not found, produce a query error.
--     * Return {operation}.
getOperation :: QueryDocument -> Maybe Name -> Maybe AST.OperationDefinition
getOperation (LoneAnonymousOperation ss _) Nothing = pure (AST.AnonymousQuery ss)
getOperation (MultipleOperations ops _) (Just name) = Map.lookup name ops
getOperation (MultipleOperations ops _) Nothing =
  case toList ops of
    [op] -> pure op
    _ -> empty
getOperation _ _ = empty

-- | Turn a parsed document into a known valid one.
--
-- The document is known to be syntactically valid, as we've got its AST.
-- Here, we confirm that it's semantically valid (modulo types).
validate :: AST.QueryDocument -> Either (NonEmpty ValidationError) QueryDocument
validate (AST.QueryDocument defns) =
  let
    (operations, fragments) = splitBy splitDefns defns
    (anonymous, named) = splitBy splitOps operations
    frags = validateFragmentDefinitions fragments
  in runValidation $
    case (anonymous, named) of
      ([], ops) -> MultipleOperations <$> mapErrors DuplicateOperation (makeMap ops) <*> frags
      ([x], []) -> LoneAnonymousOperation <$> pure x <*> frags
      _ -> throwE (MixedAnonymousOperations (length anonymous) (map fst named))

  where
    splitBy :: (a -> Either b c) -> [a] -> ([b], [c])
    splitBy f xs = partitionEithers (map f xs)

    splitDefns (AST.DefinitionOperation op) = Left op
    splitDefns (AST.DefinitionFragment frag) = Right frag

    splitOps (AST.AnonymousQuery ss) = Left ss
    splitOps q@(AST.Query (AST.Node name _ _ _)) = Right (name, q)
    splitOps m@(AST.Mutation (AST.Node name _ _ _)) = Right (name, m)

-- | A set of fragment definitions.
type FragmentDefinitions = Map Name AST.FragmentDefinition

validateFragmentDefinitions :: [AST.FragmentDefinition] -> Validation ValidationError FragmentDefinitions
validateFragmentDefinitions frags = mapErrors DuplicateFragmentDefinition (makeMap [(name, value) | value@(AST.FragmentDefinition name _ _ _) <- frags])

-- | The set of arguments for a given field, directive, etc.
--
-- Note that the 'value' can be a variable.
type ArgumentSet = Map Name AST.Value

-- | Turn a set of arguments from the AST into a guaranteed unique set of arguments.
--
-- <https://facebook.github.io/graphql/#sec-Argument-Uniqueness>
validateArguments :: [AST.Argument] -> Validation ValidationError ArgumentSet
validateArguments args = mapErrors DuplicateArgument (makeMap [(name, value) | AST.Argument name value <- args])

-- | A GraphQL selection, excluding fragment spreads (because they've been inlined).
data Selection
  = SelectionField Field
  | SelectionInlineFragment InlineFragment
  deriving (Eq, Show)

data Field
  = Field (Maybe AST.Alias) Name ArgumentSet [AST.Directive] [Selection]
  deriving (Eq, Show)

data InlineFragment
  = InlineFragment AST.TypeCondition [AST.Directive] [Selection]
  deriving (Eq, Show)

validateSelection :: FragmentDefinitions -> AST.Selection -> Validation ValidationError Selection
validateSelection fragments selection =
  case selection of
    AST.SelectionField (AST.Field alias name args directives ss) ->
      SelectionField <$> (Field alias name <$> validateArguments args <*> pure directives <*> childSegments ss)
    AST.SelectionFragmentSpread (AST.FragmentSpread name directives) ->
      -- TODO: Check that the fragment exists and store it in the selection somehow.
      notImplemented
    AST.SelectionInlineFragment (AST.InlineFragment typeCond directives ss) ->
      SelectionInlineFragment <$> (InlineFragment typeCond directives <$> childSegments ss)

  where
    childSegments = traverse (validateSelection fragments)

-- TODO: There's a chunk of duplication around "this collection of things has
-- unique names". Fix that.

-- TODO: Might be nice to have something that goes from a validated document
-- back to the AST. This would be especially useful for encoding, so we could
-- debug by looking at GraphQL rather than data types.

-- TODO: The next thing we want to do is get to valid selection sets, which
-- starts by checking that fields in a set can merge
-- (https://facebook.github.io/graphql/#sec-Field-Selection-Merging). However,
-- this depends on flattening fragments, which I really don't want to do until
-- after we've validated those.

-- | Errors arising from validating a document.
data ValidationError
  -- | 'DuplicateOperation' means there was more than one operation defined
  -- with the given name.
  --
  -- <https://facebook.github.io/graphql/#sec-Operation-Name-Uniqueness>
  = DuplicateOperation Name
  -- | 'MixedAnonymousOperations' means there was more than one operation
  -- defined in a document with an anonymous operation.
  --
  -- <https://facebook.github.io/graphql/#sec-Lone-Anonymous-Operation>
  | MixedAnonymousOperations Int [Name]
  -- | 'DuplicateArgument' means that multiple copies of the same argument was
  -- given to the same field, directive, etc.
  | DuplicateArgument Name
  -- | 'DuplicateFragmentDefinition' means that there were more than one
  -- fragment defined with the same name.
  | DuplicateFragmentDefinition Name
  deriving (Eq, Show)

-- | Identify all of the validation errors in @doc@.
--
-- An empty list means no errors.
--
-- <https://facebook.github.io/graphql/#sec-Validation>
getErrors :: AST.QueryDocument -> [ValidationError]
getErrors doc =
  case validate doc of
    Left errors -> NonEmpty.toList errors
    Right _ -> []

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
makeMap :: Ord key => [(key, value)] -> Validation key (Map key value)
makeMap entries =
  case NonEmpty.nonEmpty (findDuplicates (map fst entries)) of
    Nothing -> pure (Map.fromList entries)
    Just dups -> throwErrors dups


-- XXX: Copied from Execution
-- | Make a non-empty list. This is just an alias for the symbolic constructor.
singleton :: a -> NonEmpty a
singleton x = x :| []


-- | A 'Validation' is a value that can either be valid or have a non-empty
-- list of errors.
newtype Validation e a = Validation { runValidation :: Either (NonEmpty e) a } deriving (Eq, Show, Functor)

-- | Throw a single validation error.
throwE :: e -> Validation e a
throwE = throwErrors . singleton

-- | Throw multiple validation errors. There must be at least one.
throwErrors :: NonEmpty e -> Validation e a
throwErrors = Validation . Left

-- | Map the errors on a validation. Useful for composing validations.
mapErrors :: (e1 -> e2) -> Validation e1 a -> Validation e2 a
mapErrors f (Validation (Left es)) = Validation (Left (map f es))
mapErrors _ (Validation (Right x)) = Validation (Right x)

-- | The applicative on Validation allows multiple potentially-valid values to
-- be composed, and ensures that *all* validation errors bubble up.
instance Applicative (Validation e) where
  pure x = Validation (Right x)
  Validation (Left e1) <*> (Validation (Left e2)) = Validation (Left (e1 <> e2))
  Validation (Left e) <*> _ = Validation (Left e)
  Validation _ <*> (Validation (Left e)) = Validation (Left e)
  Validation (Right f) <*> Validation (Right x) = Validation (Right (f x))
