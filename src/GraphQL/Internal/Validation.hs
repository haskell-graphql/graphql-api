{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
-- | Transform GraphQL query documents from AST into valid structures
--
-- This corresponds roughly to the
-- [Validation](https://facebook.github.io/graphql/#sec-Validation) section of
-- the specification, except where noted.
--
-- One core difference is that this module doesn't attempt to do any
-- type-level validation, as we attempt to defer all of that to the Haskell
-- type checker.
--
-- Still missing:
--
--   * field selection merging <https://facebook.github.io/graphql/#sec-Field-Selection-Merging>
--
-- Deliberately not going to do:
--
--   * field selections on compound types <https://facebook.github.io/graphql/#sec-Field-Selections-on-Objects-Interfaces-and-Unions-Types>
--   * leaf field selections <https://facebook.github.io/graphql/#sec-Leaf-Field-Selections>
--   * argument names <https://facebook.github.io/graphql/#sec-Argument-Names>
--   * argument value type correctness <https://facebook.github.io/graphql/#sec-Argument-Values-Type-Correctness>
--   * fragment spread type existence <https://facebook.github.io/graphql/#sec-Fragment-Spread-Type-Existence>
--   * fragments on compound types <https://facebook.github.io/graphql/#sec-Fragments-On-Composite-Types>
--   * fragment spread is possible <https://facebook.github.io/graphql/#sec-Fragment-spread-is-possible>
--   * directives are defined <https://facebook.github.io/graphql/#sec-Directives-Are-Defined>
--   * directives are in valid locations <https://facebook.github.io/graphql/#sec-Directives-Are-In-Valid-Locations>
--   * variable default values are correctly typed <https://facebook.github.io/graphql/#sec-Variable-Default-Values-Are-Correctly-Typed>
--   * variables are input types <https://facebook.github.io/graphql/#sec-Variables-Are-Input-Types>
--   * all variable usages are allowed <https://facebook.github.io/graphql/#sec-All-Variable-Usages-are-Allowed>
--
-- Because all of the above rely on type checking.
module GraphQL.Internal.Validation
  ( ValidationError(..)
  , ValidationErrors
  , QueryDocument(..)
  , Selection'(..) -- TODO, can we hide this again?
  , Selection
  , InlineFragment(..)
  , validate
  , getErrors
  -- * Operating on validated documents
  , Operation
  , getVariableDefinitions
  , getSelectionSet
  , VariableDefinitions
  , VariableDefinition(..)
  , AST.Type(..)
  , Variable
  , SelectionSet
  , getFields
  , Field
  , getFieldSelectionSet
  , getResponseKey
  , FragmentSpread
  , lookupArgument
  , VariableValue
  -- * Exported for testing
  , findDuplicates
  ) where

import Protolude

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import GraphQL.Internal.Name (HasName(..), Name)
import qualified GraphQL.Internal.Syntax.AST as AST
-- Directly import things from the AST that do not need validation, so that
-- @AST.Foo@ in a type signature implies that something hasn't been validated.
import GraphQL.Internal.Syntax.AST (Alias, TypeCondition, Variable)
import GraphQL.Internal.Output (GraphQLError(..))
import GraphQL.Value
  ( Value
  , Value'
  , ConstScalar
  , UnresolvedVariableValue
  , astToVariableValue
  )

-- | A valid query document.
--
-- Construct this using 'validate' on an 'AST.QueryDocument'.
data QueryDocument value
  -- | The query document contains a single anonymous operation.
  = LoneAnonymousOperation (Operation value)
  -- | The query document contains multiple uniquely-named operations.
  | MultipleOperations (Operations value)
  deriving (Eq, Show)

data Operation value
  = Query VariableDefinitions (Directives value) (SelectionSet value)
  | Mutation VariableDefinitions (Directives value) (SelectionSet value)
  deriving (Eq, Show)

instance Functor Operation where
  fmap f (Query vars directives selectionSet) = Query vars (fmap f directives) (map (fmap f) selectionSet)
  fmap f (Mutation vars directives selectionSet) = Mutation vars (fmap f directives) (map (fmap f) selectionSet)

instance Foldable Operation where
  foldMap f (Query _ directives selectionSet) = foldMap f directives `mappend` mconcat (map (foldMap f) selectionSet)
  foldMap f (Mutation _ directives selectionSet) = foldMap f directives `mappend` mconcat (map (foldMap f) selectionSet)

instance Traversable Operation where
  traverse f (Query vars directives selectionSet) = Query vars <$> traverse f directives <*> traverse (traverse f) selectionSet
  traverse f (Mutation vars directives selectionSet) = Mutation vars <$> traverse f directives <*> traverse (traverse f) selectionSet


-- | Get the variable definitions for an operation.
getVariableDefinitions :: Operation value -> VariableDefinitions
getVariableDefinitions (Query vars _ _) = vars
getVariableDefinitions (Mutation vars _ _) = vars

-- | Get the selection set for an operation.
--
-- TODO: This doesn't return the *actual* selection set we need, because it
-- hasn't substituted variables or applied directives.
getSelectionSet :: Operation value -> SelectionSet value
getSelectionSet (Query _ _ ss) = ss
getSelectionSet (Mutation _ _ ss) = ss

-- | Type alias for 'Query' and 'Mutation' constructors of 'Operation'.
type OperationType value = VariableDefinitions -> Directives value -> SelectionSet value -> Operation value

type Operations value = Map Name (Operation value)

-- | Turn a parsed document into a known valid one.
--
-- The document is known to be syntactically valid, as we've got its AST.
-- Here, we confirm that it's semantically valid (modulo types).
validate :: AST.QueryDocument -> Either (NonEmpty ValidationError) (QueryDocument VariableValue)
validate (AST.QueryDocument defns) = runValidator $ do
  let (operations, fragments) = splitBy splitDefns defns
  let (anonymous, named) = splitBy splitOps operations
  (frags, visitedFrags) <- resolveFragmentDefinitions =<< validateFragmentDefinitions fragments
  case (anonymous, named) of
    ([], ops) -> do
      (validOps, usedFrags) <- runStateT (validateOperations frags ops) mempty
      assertAllFragmentsUsed frags (visitedFrags <> usedFrags)
      resolvedOps <- traverse validateOperation validOps
      pure (MultipleOperations resolvedOps)
    ([x], []) -> do
      (ss, usedFrags) <- runStateT (validateSelectionSet frags x) mempty
      assertAllFragmentsUsed frags (visitedFrags <> usedFrags)
      validValuesSS <- traverse validateValues ss
      resolvedValuesSS <- traverse (resolveVariables emptyVariableDefinitions) validValuesSS
      pure (LoneAnonymousOperation (Query emptyVariableDefinitions emptyDirectives resolvedValuesSS))
    _ -> throwE (MixedAnonymousOperations (length anonymous) (map fst named))

  where
    splitBy :: (a -> Either b c) -> [a] -> ([b], [c])
    splitBy f xs = partitionEithers (map f xs)

    splitDefns (AST.DefinitionOperation op) = Left op
    splitDefns (AST.DefinitionFragment frag) = Right frag

    splitOps (AST.AnonymousQuery ss) = Left ss
    splitOps (AST.Query node@(AST.Node name _ _ _)) = Right (name, (Query, node))
    splitOps (AST.Mutation node@(AST.Node name _ _ _)) = Right (name, (Mutation, node))

    assertAllFragmentsUsed :: Fragments value -> Set Name -> Validation ()
    assertAllFragmentsUsed fragments used =
      let unused = Map.keysSet fragments `Set.difference` used
      in unless (Set.null unused) (throwE (UnusedFragments unused))

-- * Operations

validateOperations :: Fragments AST.Value -> [(Name, (OperationType AST.Value, AST.Node))] -> StateT (Set Name) Validation (Operations AST.Value)
validateOperations fragments ops = do
  deduped <- lift (mapErrors DuplicateOperation (makeMap ops))
  traverse validateNode deduped
  where
    validateNode (operationType, AST.Node _ vars directives ss) =
      operationType <$> lift (validateVariableDefinitions vars)
                    <*> lift (validateDirectives directives)
                    <*> validateSelectionSet fragments ss

-- TODO: Either make operation type (Query, Mutation) a parameter of an
-- Operation constructor or give all the fields accessors. This duplication is
-- driving me batty.
validateOperation :: Operation AST.Value -> Validation (Operation VariableValue)
validateOperation (Query vars directives selectionSet) = do
  validValues <- Query vars <$> validateValues directives <*> traverse validateValues selectionSet
  -- Instead of doing this, we could build up a list of used variables as we
  -- resolve them.
  let usedVariables = getVariables validValues
  let definedVariables = getDefinedVariables vars
  let unusedVariables = definedVariables `Set.difference` usedVariables
  unless (Set.null unusedVariables) $ throwE (UnusedVariables unusedVariables)
  resolveVariables vars validValues
validateOperation (Mutation vars directives selectionSet) = do
  validValues <- Mutation vars <$> validateValues directives <*> traverse validateValues selectionSet
  -- Instead of doing this, we could build up a list of used variables as we
  -- resolve them.
  let usedVariables = getVariables validValues
  let definedVariables = getDefinedVariables vars
  let unusedVariables = definedVariables `Set.difference` usedVariables
  unless (Set.null unusedVariables) $ throwE (UnusedVariables unusedVariables)
  resolveVariables vars validValues

-- * Arguments

-- | The set of arguments for a given field, directive, etc.
--
-- Note that the 'value' can be a variable.
newtype Arguments value = Arguments (Map Name value) deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Turn a set of arguments from the AST into a guaranteed unique set of arguments.
--
-- <https://facebook.github.io/graphql/#sec-Argument-Uniqueness>
validateArguments :: [AST.Argument] -> Validation (Arguments AST.Value)
validateArguments args = Arguments <$> mapErrors DuplicateArgument (makeMap [(name, value) | AST.Argument name value <- args])

-- * Selections

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
-- so than including the resolved fragment in the type. Thus, @spread@ will be
-- 'FragmentSpread', following this module's convention that unadorned names
-- imply that everything is valid.

type SelectionSet value = [Selection value]

type Selection value = Selection' FragmentSpread value

-- | A GraphQL selection.
data Selection' (spread :: * -> *) value
  = SelectionField (Field' spread value)
  | SelectionFragmentSpread (spread value)
  | SelectionInlineFragment (InlineFragment spread value)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Get all of the fields directly inside the given selection set.
--
-- TODO: This ignores fragments, whereas it should actually do something with
-- them.
--
-- TODO: At this point, we ought to know that field names are unique. As such,
-- we should return an ordered map of Name to Fields, rather than a bland
-- list.
getFields :: SelectionSet value -> [Field' FragmentSpread value]
getFields ss = [field | SelectionField field <- ss]

-- | A field in a selection set, which itself might have children which might
-- have fragment spreads.
data Field' spread value
  = Field' (Maybe Alias) Name (Arguments value) (Directives value) [Selection' spread value]
  deriving (Eq, Show)

-- | Get the response key of a field.
--
-- \"A field’s response key is its alias if an alias is provided, and it is
-- otherwise the field’s name.\"
--
-- <https://facebook.github.io/graphql/#sec-Field-Alias>
getResponseKey :: Field' spread value -> Name
getResponseKey (Field' alias name _ _ _) = fromMaybe name alias

instance HasName (Field' spread value) where
  getName (Field' _ name _ _ _) = name

instance Functor spread => Functor (Field' spread) where
  fmap f (Field' alias name arguments directives selectionSet) =
    Field' alias name (fmap f arguments) (fmap f directives) (map (fmap f) selectionSet)

instance Foldable spread => Foldable (Field' spread) where
  foldMap f (Field' _ _ arguments directives selectionSet) =
    mconcat [ foldMap f arguments
            , foldMap f directives
            , mconcat (map (foldMap f) selectionSet)
            ]

instance Traversable spread => Traversable (Field' spread) where
  traverse f (Field' alias name arguments directives selectionSet) =
    Field' alias name <$> traverse f arguments
                      <*> traverse f directives
                      <*> traverse (traverse f) selectionSet

type Field value = Field' FragmentSpread value

-- | Get the value of an argument in a field.
lookupArgument :: Field' spread value -> Name -> Maybe value
lookupArgument (Field' _ _ (Arguments args) _ _) name = Map.lookup name args

-- | Get the selection set within a field.
getFieldSelectionSet :: Field' spread value -> [Selection' spread value]
getFieldSelectionSet (Field' _ _ _ _ ss) = ss

-- | A fragment spread that has a valid set of directives, but may or may not
-- refer to a fragment that actually exists.
data UnresolvedFragmentSpread value
  = UnresolvedFragmentSpread Name (Directives value)
  deriving (Eq, Show, Functor)

instance Foldable UnresolvedFragmentSpread where
  foldMap f (UnresolvedFragmentSpread _ directives) = foldMap f directives

instance Traversable UnresolvedFragmentSpread where
  traverse f (UnresolvedFragmentSpread name directives) = UnresolvedFragmentSpread name <$> traverse f directives

-- | A fragment spread that refers to fragments which are known to exist.
data FragmentSpread value
  = FragmentSpread Name (Directives value) (FragmentDefinition FragmentSpread value)
  deriving (Eq, Show)

instance Functor FragmentSpread where
  fmap f (FragmentSpread name directives definition) = FragmentSpread name (fmap f directives) (fmap f definition)

instance Foldable FragmentSpread where
  foldMap f (FragmentSpread _ directives fragment) = foldMap f directives `mappend` foldMap f fragment

instance Traversable FragmentSpread where
  traverse f (FragmentSpread name directives definition) =
    FragmentSpread name <$> traverse f directives <*> traverse f definition

-- | An inline fragment, which itself can contain fragment spreads.
data InlineFragment spread value
  = InlineFragment (Maybe TypeCondition) (Directives value) [Selection' spread value]
  deriving (Eq, Show)

instance Functor spread => Functor (InlineFragment spread) where
  fmap f (InlineFragment typeCond directives selectionSet) =
    InlineFragment typeCond (fmap f directives) (map (fmap f) selectionSet)

instance Foldable spread => Foldable (InlineFragment spread) where
  foldMap f (InlineFragment _ directives selectionSet) =
    foldMap f directives `mappend` mconcat (map (foldMap f) selectionSet)

instance Traversable spread => Traversable (InlineFragment spread) where
  traverse f (InlineFragment typeCond directives selectionSet) =
    InlineFragment typeCond <$> traverse f directives
                            <*> traverse (traverse f) selectionSet

-- | Traverse through every fragment spread in a selection.
--
-- The given function @f@ is applied to each fragment spread. The rest of the
-- selection remains unchanged.
--
-- Note that this is essentially a definition of 'Traversable' for
-- 'Selection'. However, we probably also want to have other kinds of
-- traversals (e.g. for transforming values), so best not to bless one kind
-- with a type class.
traverseFragmentSpreads :: Applicative f => (a value -> f (b value)) -> Selection' a value -> f (Selection' b value)
traverseFragmentSpreads f selection =
  case selection of
    SelectionField (Field' alias name args directives ss) ->
      SelectionField <$> (Field' alias name args directives <$> childSegments ss)
    SelectionFragmentSpread x ->
      SelectionFragmentSpread <$> f x
    SelectionInlineFragment (InlineFragment typeCond directives ss) ->
      SelectionInlineFragment <$> (InlineFragment typeCond directives <$> childSegments ss)
  where
    childSegments = traverse (traverseFragmentSpreads f)

-- | Ensure a selection has valid arguments and directives.
validateSelection :: AST.Selection -> Validation (Selection' UnresolvedFragmentSpread AST.Value)
validateSelection selection =
  case selection of
    AST.SelectionField (AST.Field alias name args directives ss) ->
      SelectionField <$> (Field' alias name <$> validateArguments args <*> validateDirectives directives <*> childSegments ss)
    AST.SelectionFragmentSpread (AST.FragmentSpread name directives) ->
      SelectionFragmentSpread <$> (UnresolvedFragmentSpread name <$> validateDirectives directives)
    AST.SelectionInlineFragment (AST.InlineFragment typeCond directives ss) ->
      SelectionInlineFragment <$> (InlineFragment typeCond <$> validateDirectives directives <*> childSegments ss)
  where
    childSegments = traverse validateSelection

-- | Resolve the fragment references in a selection, accumulating a set of
-- the fragment names that we have resolved.
--
-- We're doing a standard depth-first traversal of fragment references, where
-- references are by name, so the set of names can be thought of as a record
-- of visited references.
resolveSelection :: Fragments a -> Selection' UnresolvedFragmentSpread a -> StateT (Set Name) Validation (Selection' FragmentSpread a)
resolveSelection fragments = traverseFragmentSpreads resolveFragmentSpread
  where
    resolveFragmentSpread (UnresolvedFragmentSpread name directive) = do
      case Map.lookup name fragments of
        Nothing -> lift (throwE (NoSuchFragment name))
        Just fragment -> do
          modify (Set.insert name)
          pure (FragmentSpread name directive fragment)

validateSelectionSet :: Fragments AST.Value -> [AST.Selection] -> StateT (Set Name) Validation (SelectionSet AST.Value)
validateSelectionSet fragments selections = do
  unresolved <- lift (traverse validateSelection selections)
  resolved <- traverse (resolveSelection fragments) unresolved
  -- TODO: Check that the fields are mergable.
  pure resolved

-- * Fragment definitions

-- | A validated fragment definition.
--
-- @spread@ indicates whether references to other fragment definitions have
-- been resolved.
data FragmentDefinition spread value
  = FragmentDefinition Name TypeCondition (Directives value) [Selection' spread value]
  deriving (Eq, Show)

type Fragments value = Map Name (FragmentDefinition FragmentSpread value)

instance Functor spread => Functor (FragmentDefinition spread) where
  fmap f (FragmentDefinition name typeCond directives selectionSet) =
    FragmentDefinition name typeCond (fmap f directives) (map (fmap f) selectionSet)

instance Foldable spread => Foldable (FragmentDefinition spread) where
  foldMap f (FragmentDefinition _ _ directives selectionSet) =
    foldMap f directives `mappend` mconcat (map (foldMap f) selectionSet)

instance Traversable spread => Traversable (FragmentDefinition spread) where
  traverse f (FragmentDefinition name typeCond directives selectionSet) =
    FragmentDefinition name typeCond <$> traverse f directives
                                     <*> traverse (traverse f) selectionSet

-- | Ensure fragment definitions are uniquely named, and that their arguments
-- and directives are sane.
--
-- <https://facebook.github.io/graphql/#sec-Fragment-Name-Uniqueness>
validateFragmentDefinitions :: [AST.FragmentDefinition] -> Validation (Map Name (FragmentDefinition UnresolvedFragmentSpread AST.Value))
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
resolveFragmentDefinitions :: Map Name (FragmentDefinition UnresolvedFragmentSpread value) -> Validation (Fragments value, Set Name)
resolveFragmentDefinitions allFragments =
  splitResult <$> traverse resolveFragment allFragments
  where
    -- The result of our computation is a map from names of fragment
    -- definitions to the resolved fragment and visited names. We want to
    -- split out the visited names and combine them so that later we can
    -- report on the _un_visited names.
    splitResult mapWithVisited = (map fst mapWithVisited, foldMap snd mapWithVisited)

    -- | Resolves all references to fragments in a fragment definition,
    -- returning the resolved fragment and a set of visited names.
    resolveFragment frag = runStateT (resolveFragment' frag) mempty

    resolveFragment' (FragmentDefinition name cond directives ss) =
      FragmentDefinition name cond directives <$> traverse (traverseFragmentSpreads resolveSpread) ss

    resolveSpread (UnresolvedFragmentSpread name directives) = do
      visited <- Set.member name <$> get
      when visited (lift (throwE (CircularFragmentSpread name)))
      case Map.lookup name allFragments of
        Nothing -> lift (throwE (NoSuchFragment name))
        Just definition -> do
          modify (Set.insert name)
          FragmentSpread name directives <$> resolveFragment' definition

-- * Variables

-- | Defines a variable within the context of an operation.
--
-- See <https://facebook.github.io/graphql/#sec-Language.Variables>
data VariableDefinition
  = VariableDefinition
    { variable :: Variable -- ^ The name of the variable
    , variableType :: AST.Type -- ^ The type of the variable
    , defaultValue :: Maybe Value -- ^ An optional default value for the variable
    } deriving (Eq, Ord, Show)

type VariableDefinitions = Map Variable VariableDefinition

getDefinedVariables :: VariableDefinitions -> Set Variable
getDefinedVariables = Map.keysSet

-- | A GraphQL value which might contain some defined variables.
type VariableValue = Value' (Either VariableDefinition ConstScalar)

emptyVariableDefinitions :: VariableDefinitions
emptyVariableDefinitions = mempty

-- | Ensure that a set of variable definitions is valid.
validateVariableDefinitions :: [AST.VariableDefinition] -> Validation VariableDefinitions
validateVariableDefinitions vars = do
  validatedDefns <- traverse validateVariableDefinition vars
  let items = [ (variable defn, defn) | defn <- validatedDefns]
  mapErrors DuplicateVariableDefinition (makeMap items)

-- | Ensure that a variable definition is a valid one.
validateVariableDefinition :: AST.VariableDefinition -> Validation VariableDefinition
validateVariableDefinition (AST.VariableDefinition name varType value) =
  VariableDefinition name varType <$> traverse validateDefaultValue value

-- | Ensure that a default value contains no variables.
validateDefaultValue :: AST.DefaultValue -> Validation Value
validateDefaultValue defaultValue =
  case astToVariableValue defaultValue of
    Nothing -> throwE $ InvalidValue defaultValue
    Just value ->
      for value $
      \case
        Left _ -> throwE $ InvalidDefaultValue defaultValue
        Right constant -> pure constant


-- | Get all the variables referred to in a thing what contains variables.
getVariables :: Foldable f => f UnresolvedVariableValue -> Set Variable
getVariables = foldMap valueToVariable
  where
    valueToVariable = foldMap (either Set.singleton (const Set.empty))

-- | Make sure all the values are valid.
validateValues :: Traversable f => f AST.Value -> Validation (f UnresolvedVariableValue)
validateValues = traverse toVariableValue
  where
    toVariableValue astValue =
      case astToVariableValue astValue of
        Just value -> pure value
        Nothing -> throwE (InvalidValue astValue)

-- | Make sure each variable has a definition, and each definition a variable.
resolveVariables :: Traversable f => VariableDefinitions -> f UnresolvedVariableValue -> Validation (f VariableValue)
resolveVariables definitions = traverse resolveVariableValue
  where
    resolveVariableValue = traverse resolveVariable
    resolveVariable (Left variable) =
      case Map.lookup variable definitions of
        Nothing -> throwE (UndefinedVariable variable)
        Just defn -> pure (Left defn)
    resolveVariable (Right constant) = pure (Right constant)


-- * Directives

-- | A directive is a way of changing the run-time behaviour
newtype Directives value = Directives (Map Name (Arguments value)) deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

emptyDirectives :: Directives value
emptyDirectives = Directives Map.empty

-- | Ensure that the directives in a given place are valid.
--
-- Doesn't check to see if directives are defined & doesn't check to see if
-- they are in valid locations, because we don't have access to the schema at
-- this point.
--
-- <https://facebook.github.io/graphql/#sec-Directives-Are-Unique-Per-Location>
validateDirectives :: [AST.Directive] -> Validation (Directives AST.Value)
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
  -- | There were multiple variables defined with the same name.
  | DuplicateVariableDefinition Variable
  -- | 'CircularFragmentSpread' means that a fragment definition contains a
  -- fragment spread that itself is a fragment definition that contains a
  -- fragment spread referring to the /first/ fragment spread.
  | CircularFragmentSpread Name
  -- | 'UnusedFragments' means that fragments were defined that weren't used.
  -- <https://facebook.github.io/graphql/#sec-Fragments-Must-Be-Used>
  | UnusedFragments (Set Name)
  -- | Variables were defined without being used.
  -- <https://facebook.github.io/graphql/#sec-All-Variables-Used>
  | UnusedVariables (Set Variable)
  -- | A variable was used without being defined.
  -- <https://facebook.github.io/graphql/#sec-All-Variable-Uses-Defined>
  | UndefinedVariable Variable
  -- | Value in AST wasn't valid.
  | InvalidValue AST.Value
  -- | Default value in AST contained variables.
  | InvalidDefaultValue AST.Value
  deriving (Eq, Show)

instance GraphQLError ValidationError where
  formatError (DuplicateOperation name) = "More than one operation named '" <> show name <> "'"
  formatError (MixedAnonymousOperations n names)
    | n > 1 && null names = "Multiple anonymous operations defined. Found " <> show n
    | otherwise = "Document contains both anonymous operations (" <> show n <> ") and named operations (" <> show names <> ")"
  formatError (DuplicateArgument name) = "More than one argument named '" <> show name <> "'"
  formatError (DuplicateFragmentDefinition name) = "More than one fragment named '" <> show name <> "'"
  formatError (NoSuchFragment name) = "No fragment named '" <> show name <> "'"
  formatError (DuplicateDirective name) = "More than one directive named '" <> show name <> "'"
  formatError (DuplicateVariableDefinition name) = "More than one variable defined with name '" <> show name <> "'"
  formatError (CircularFragmentSpread name) = "Fragment '" <> show name <> "' contains a fragment spread that refers back to itself."
  formatError (UnusedFragments names) = "Fragments defined but not used: " <> show names
  formatError (UnusedVariables names) = "Variables defined but not used: " <> show names
  formatError (UndefinedVariable variable) = "No definition for variable: " <> show variable
  formatError (InvalidValue value) = "Invalid value (maybe an object has duplicate field names?): " <> show value
  formatError (InvalidDefaultValue value) = "Invalid default value, contains variables: " <> show value

type ValidationErrors = NonEmpty ValidationError

-- | Type alias for our most common kind of validator.
type Validation = Validator ValidationError

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
makeMap :: Ord key => [(key, value)] -> Validator key (Map key value)
makeMap entries =
  case NonEmpty.nonEmpty (findDuplicates (map fst entries)) of
    Nothing -> pure (Map.fromList entries)
    Just dups -> throwErrors dups

-- * Error handling

-- | A 'Validator' is a value that can either be valid or have a non-empty
-- list of errors.
newtype Validator e a = Validator { runValidator :: Either (NonEmpty e) a } deriving (Eq, Show, Functor, Monad)

-- | Throw a single validation error.
throwE :: e -> Validator e a
throwE e = throwErrors (e :| [])

-- | Throw multiple validation errors. There must be at least one.
throwErrors :: NonEmpty e -> Validator e a
throwErrors = Validator . Left

-- | Map over each individual error on a validation. Useful for composing
-- validations.
--
-- This is /somewhat/ like 'first', but 'Validator' is not, and cannot be, a
-- 'Bifunctor', because the left-hand side is specialized to @NonEmpty e@,
-- rather than plain @e@. Also, whatever function were passed to 'first' would
-- get the whole non-empty list, whereas 'mapErrors' works on one element at a
-- time.
--
-- >>> mapErrors (+1) (pure "hello")
-- Validator {runValidator = Right "hello"}
-- >>> mapErrors (+1) (throwE 2)
-- Validator {runValidator = Left (3 :| [])}
-- >>> mapErrors (+1) (throwErrors (NonEmpty.fromList [3, 5]))
-- Validator {runValidator = Left (4 :| [6])}
mapErrors :: (e1 -> e2) -> Validator e1 a -> Validator e2 a
mapErrors f (Validator (Left es)) = Validator (Left (map f es))
mapErrors _ (Validator (Right x)) = Validator (Right x)

-- | The applicative on Validator allows multiple potentially-valid values to
-- be composed, and ensures that *all* validation errors bubble up.
instance Applicative (Validator e) where
  pure x = Validator (Right x)
  Validator (Left e1) <*> (Validator (Left e2)) = Validator (Left (e1 <> e2))
  Validator (Left e) <*> _ = Validator (Left e)
  Validator _ <*> (Validator (Left e)) = Validator (Left e)
  Validator (Right f) <*> Validator (Right x) = Validator (Right (f x))
