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
import qualified Data.Set as Set
import qualified GraphQL.Internal.AST as AST
-- Directly import things from the AST that do not need validation, so that
-- @AST.Foo@ in a type signature implies that something hasn't been validated.
import GraphQL.Internal.AST (Name)

-- | A valid query document.
--
-- Construct this using 'validate' on an 'AST.QueryDocument'.
data QueryDocument
  -- | The query document contains a single anonymous operation.
  = LoneAnonymousOperation AST.SelectionSet (Map Name (FragmentDefinition FragmentSpread))
  -- | The query document contains multiple uniquely-named operations.
  | MultipleOperations (Map Name AST.OperationDefinition) (Map Name (FragmentDefinition FragmentSpread))
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
validate (AST.QueryDocument defns) = runValidation $ do
  let (operations, fragments) = splitBy splitDefns defns
  let (anonymous, named) = splitBy splitOps operations
  -- Deliberately don't unpack this monadically so we continue processing and
  -- get all the errors.
  let frags = fst <$> (resolveFragmentDefinitions =<< validateFragmentDefinitions fragments)
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

-- * Arguments

-- | The set of arguments for a given field, directive, etc.
--
-- Note that the 'value' can be a variable.
type ArgumentSet = Map Name AST.Value

-- | Turn a set of arguments from the AST into a guaranteed unique set of arguments.
--
-- <https://facebook.github.io/graphql/#sec-Argument-Uniqueness>
validateArguments :: [AST.Argument] -> Validation ValidationError ArgumentSet
validateArguments args = mapErrors DuplicateArgument (makeMap [(name, value) | AST.Argument name value <- args])

-- * Selections and Fragments

-- $fragmentSpread
--
-- The @spread@ type variable is for the type used to "fragment spreads", i.e.
-- references to fragments. It's a variable because we do multiple traversals
-- of the selection graph.
--
-- The first pass (see 'validateSelection') ensures all the arguments and
-- directives are valid. This is applied to all selections, including those
-- that make up fragment definitions (see 'validateFragmentDefinitions'). At
-- this stage, @spread@ will be 'UnresolvedFragmentSpread'.
--
-- Once we have a known-good map of fragment definitions, we can do the next
-- phase of validation, which checks that references to fragments exist, that
-- all fragments are used, and that we don't have circular references.
--
-- This is encoded as a type variable because we want to provide evidence that
-- references in fragment spreads can be resolved, and what better way to do
-- so than including the resolved fragment in the type. Thus, @spread will be
-- 'FragmentSpread', following this module's convention that unadorned names
-- imply that everything is valid.

-- | A GraphQL selection.
data Selection spread
  = SelectionField (Field spread)
  | SelectionFragmentSpread spread
  | SelectionInlineFragment (InlineFragment spread)
  deriving (Eq, Show)

-- TODO: I'm pretty sure we want to change all of these [Selection spread]
-- lists to Maps.

-- | A field in a selection set, which itself might have children which might
-- have fragment spreads.
data Field spread
  = Field (Maybe AST.Alias) Name ArgumentSet Directives [Selection spread]
  deriving (Eq, Show)

-- | A fragment spread that has a valid set of directives, but may or may not
-- refer to a fragment that actually exists.
data UnresolvedFragmentSpread
  = UnresolvedFragmentSpread Name Directives
  deriving (Eq, Show)

-- | A fragment spread that refers to fragments which are known to exist.
data FragmentSpread
  = FragmentSpread Name Directives (FragmentDefinition FragmentSpread)
  deriving (Eq, Show)

-- | An inline fragment, which itself can contain fragment spreads.
data InlineFragment spread
  = InlineFragment AST.TypeCondition Directives [Selection spread]
  deriving (Eq, Show)

-- | Ensure a selection has valid arguments and directives.
validateSelection :: AST.Selection -> Validation ValidationError (Selection UnresolvedFragmentSpread)
validateSelection selection =
  case selection of
    AST.SelectionField (AST.Field alias name args directives ss) ->
      SelectionField <$> (Field alias name <$> validateArguments args <*> validateDirectives directives <*> childSegments ss)
    AST.SelectionFragmentSpread (AST.FragmentSpread name directives) ->
      SelectionFragmentSpread <$> (UnresolvedFragmentSpread name <$> validateDirectives directives)
    AST.SelectionInlineFragment (AST.InlineFragment typeCond directives ss) ->
      SelectionInlineFragment <$> (InlineFragment typeCond <$> validateDirectives directives <*> childSegments ss)
  where
    childSegments = traverse validateSelection

-- | Traverse through every fragment spread in a selection.
--
-- The given function @f@ is applied to each fragment spread. The rest of the
-- selection remains unchanged.
--
-- TODO: This is basically a definition of 'Traversable' for 'Selection'.
-- Change it to be so once we've proven that this crazy type variable thing
-- works out.
traverseFragmentSpreads :: Applicative f => (a -> f b) -> Selection a -> f (Selection b)
traverseFragmentSpreads f selection =
  case selection of
    SelectionField (Field alias name args directives ss) ->
      SelectionField <$> (Field alias name args directives <$> childSegments ss)
    SelectionFragmentSpread x -> SelectionFragmentSpread <$> f x
    SelectionInlineFragment (InlineFragment typeCond directives ss) ->
      SelectionInlineFragment <$> (InlineFragment typeCond directives <$> childSegments ss)
  where
    childSegments = traverse (traverseFragmentSpreads f)

-- | Resolve the fragment references in a selection, accumulating a set of
-- visited fragment names.
resolveSelection :: Map Name (FragmentDefinition FragmentSpread) -> Selection UnresolvedFragmentSpread -> StateT (Set Name) (Validation ValidationError) (Selection FragmentSpread)
resolveSelection fragments selection = traverseFragmentSpreads resolveFragmentSpread selection
  where
    resolveFragmentSpread :: UnresolvedFragmentSpread -> StateT (Set Name) (Validation ValidationError) FragmentSpread
    resolveFragmentSpread (UnresolvedFragmentSpread name directive) = do
      case Map.lookup name fragments of
        Nothing -> lift (throwE (NoSuchFragment name))
        Just fragment -> do
          modify (Set.insert name)
          pure (FragmentSpread name directive fragment)

-- | A validated fragment definition.
--
-- @spread@ indicates whether references to other fragment definitions have
-- been resolved.
data FragmentDefinition spread
  = FragmentDefinition Name AST.TypeCondition Directives [Selection spread]
  deriving (Eq, Show)

-- | Ensure fragment definitions are uniquely named, and that their arguments
-- and directives are sane.
--
-- <https://facebook.github.io/graphql/#sec-Fragment-Name-Uniqueness>
validateFragmentDefinitions :: [AST.FragmentDefinition] -> Validation ValidationError (Map Name (FragmentDefinition UnresolvedFragmentSpread))
validateFragmentDefinitions frags = do
  defns <- traverse validateFragmentDefinition frags
  mapErrors DuplicateFragmentDefinition (makeMap [(name, value) | value@(FragmentDefinition name _ _ _) <- defns])
  where
    validateFragmentDefinition (AST.FragmentDefinition name cond directives ss) =
      FragmentDefinition name cond <$> validateDirectives directives <*> traverse validateSelection ss

-- | Resolve all references to fragments inside fragment definitions.
--
-- Guarantees that fragment spreads refer to fragments that have been defined,
-- and that there are no circular references.
--
-- Returns the resolved fragment definitions and a set of the names of all
-- defined fragments that were referred to by other fragments. This is to be
-- used to guarantee that all defined fragments are used (c.f.
-- <https://facebook.github.io/graphql/#sec-Fragments-Must-Be-Used>).
--
-- <https://facebook.github.io/graphql/#sec-Fragment-spread-target-defined>
-- <https://facebook.github.io/graphql/#sec-Fragment-spreads-must-not-form-cycles>
resolveFragmentDefinitions :: Map Name (FragmentDefinition UnresolvedFragmentSpread) -> Validation ValidationError (Map Name (FragmentDefinition FragmentSpread), Set Name)
resolveFragmentDefinitions allFragments =
  splitResult <$> traverse resolveFragment allFragments
  where
    -- The result of our computation is a map from names of fragment
    -- definitions to the resolved fragment and visited names. We want to
    -- split out the visited names and combine them so that later we can
    -- report on the _un_visited names.
    splitResult :: Map Name (FragmentDefinition FragmentSpread, Set Name) -> (Map Name (FragmentDefinition FragmentSpread), Set Name)
    splitResult mapWithVisited = (map fst mapWithVisited, foldMap snd mapWithVisited)

    -- | Resolves all references to fragments in a fragment definition,
    -- returning the resolved fragment and a set of visited names.
    resolveFragment :: FragmentDefinition UnresolvedFragmentSpread -> Validation ValidationError (FragmentDefinition FragmentSpread, Set Name)
    resolveFragment frag = runStateT (resolveFragment' frag) mempty

    resolveFragment' :: FragmentDefinition UnresolvedFragmentSpread -> StateT (Set Name) (Validation ValidationError) (FragmentDefinition FragmentSpread)
    resolveFragment' (FragmentDefinition name cond directives ss) =
      FragmentDefinition name cond directives <$> traverse (traverseFragmentSpreads resolveSpread) ss

    resolveSpread :: UnresolvedFragmentSpread -> StateT (Set Name) (Validation ValidationError) FragmentSpread
    resolveSpread (UnresolvedFragmentSpread name directives) = do
      visited <- Set.member name <$> get
      when visited (lift (throwE (CircularFragmentSpread name)))
      case Map.lookup name allFragments of
        Nothing -> lift (throwE (NoSuchFragment name))
        Just definition -> do
          modify (Set.insert name)
          FragmentSpread name directives <$> resolveFragment' definition

-- * Directives

-- | A directive is a way of changing the run-time behaviour
newtype Directives = Directives (Map Name ArgumentSet) deriving (Eq, Show)

-- | Ensure that the directives in a given place are valid.
--
-- Doesn't check to see if directives are defined & doesn't check to see if
-- they are in valid locations, because we don't have access to the schema at
-- this point.
--
-- <https://facebook.github.io/graphql/#sec-Directives-Are-Unique-Per-Location>
validateDirectives :: [AST.Directive] -> Validation ValidationError Directives
validateDirectives directives = do
  items <- traverse validateDirective directives
  Directives <$> mapErrors DuplicateDirective (makeMap items)
  where
    validateDirective (AST.Directive name args) = (,) name <$> validateArguments args

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

-- * Validation errors

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
  -- | 'NoSuchFragment' means there was a reference to a fragment in a
  -- fragment spread but we couldn't find any fragment with that name.
  | NoSuchFragment Name
  -- | 'DuplicateDirective' means there were two copies of the same directive
  -- given in the same place.
  --
  -- <https://facebook.github.io/graphql/#sec-Directives-Are-Unique-Per-Location>
  | DuplicateDirective Name
  -- | 'CircularFragmentSpread' means that a fragment definition contains a
  -- fragment spread that itself is a fragment definition that contains a
  -- fragment spread referring to the /first/ fragment spread.
  | CircularFragmentSpread Name
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

-- * Helper functions

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

-- * Error handling

-- | A 'Validation' is a value that can either be valid or have a non-empty
-- list of errors.
newtype Validation e a = Validation { runValidation :: Either (NonEmpty e) a } deriving (Eq, Show, Functor)

-- | Throw a single validation error.
throwE :: e -> Validation e a
throwE e = throwErrors (e :| [])

-- | Throw multiple validation errors. There must be at least one.
throwErrors :: NonEmpty e -> Validation e a
throwErrors = Validation . Left

-- | Map over each individual error on a validation. Useful for composing
-- validations.
--
-- This is /somewhat/ like 'first', but 'Validation' is not, and cannot be, a
-- 'Bifunctor', because the left-hand side is specialized to @NonEmpty e@,
-- rather than plain @e@. Also, whatever function were passed to 'first' would
-- get the whole non-empty list, whereas 'mapErrors' works on one element at a
-- time.
--
-- >>> mapErrors (+1) (pure "hello")
-- Validation {runValidation = Right "hello"}
-- >>> mapErrors (+1) (throwE 2)
-- Validation {runValidation = Left (3 :| [])}
-- >>> mapErrors (+1) (throwErrors (NonEmpty.fromList [3, 5]))
-- Validation {runValidation = Left (4 :| [6])}
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

-- | The monad on Validation stops processing once errors have been raised.
instance Monad (Validation e) where
  Validation (Left e) >>= _ = Validation (Left e)
  Validation (Right r) >>= f = f r
