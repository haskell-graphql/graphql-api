{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-} -- nicer type errors in some cases
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- for TypeError

module GraphQL.Resolver
  ( ResolverError(..) -- XXX: Exporting constructor for tests. Not sure if that's what we really want.
  , HasGraph(..)
  , (:<>)(..)
  , BuildFieldResolver(..)
  , Defaultable(..)
  , Result(..)
  , unionValue
  ) where

-- TODO (probably incomplete, the spec is large)
-- - input objects - I'm not super clear from the spec on how
--   they differ from normal objects.
-- - "extend type X" is used in examples in the spec but it's not
--   explained anywhere?
-- - Directives (https://facebook.github.io/graphql/#sec-Type-System.Directives)
-- - Enforce non-empty lists (might only be doable via value-level validation)

import Protolude hiding (Enum, TypeError)
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage(..), Symbol, symbolVal)
import qualified GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import GraphQL.API
  ( (:>)
  , HasAnnotatedType(..)
  , HasAnnotatedInputType(..)
  )
import qualified GraphQL.API as API
import qualified GraphQL.Value as GValue
import GraphQL.Value (Name, Value)
import GraphQL.Value.FromValue (FromValue(..))
import GraphQL.Value.ToValue (ToValue(..))
import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.Output (GraphQLError(..))
import GraphQL.Internal.Schema (HasName(..))
import GraphQL.Internal.Validation
  ( SelectionSet
  , Selection'(..)
  , InlineFragment(..)
  , FragmentSpread
  , Field
  , getFields
  , getFieldSelectionSet
  , lookupArgument
  )

data ResolverError
  -- | There was a problem in the schema. Server-side problem.
  = SchemaError AST.NameError
  -- | Couldn't find the requested field in the object. A client-side problem.
  | FieldNotFoundError (Field Value)
  -- | No value provided for name, and no default specified. Client-side problem.
  | ValueMissing Name
  -- | Could not translate value into Haskell. Probably a client-side problem.
  | InvalidValue Name Text
  -- | Found duplicate fields in set.
  | DuplicateFields [ResolveFieldResult]  -- TODO: Catch this in validation
  -- | We tried to use an inline fragment with a name that the union
  -- type does not support.
  | UnionTypeNotFound Name (SelectionSet Value)
  -- | We found more than one inline fragment matching the given type condition.
  | MultipleInlineFragmentsForType Name [InlineFragment FragmentSpread Value]
  deriving (Show, Eq)

instance GraphQLError ResolverError where
  formatError (SchemaError e) =
    "Schema error: " <> formatError e
  formatError (FieldNotFoundError field) =
    "Could not find value for field: " <> show field
  formatError (ValueMissing name) =
    "No value provided for " <> show name <> ", and no default specified."
  formatError (InvalidValue name text) =
    "Could not coerce " <> show name <> " to valid value: " <> text
  -- TODO: format 'result' nicely
  formatError (DuplicateFields result) =
    "Duplicate fields requested: " <> show result
  formatError (UnionTypeNotFound unionTypeName selectionSet) =
    "No inline fragment for " <> show unionTypeName
    <> " (e.g. '... on " <> show unionTypeName <> "') found in selection set: "
    <> show selectionSet
  formatError (MultipleInlineFragmentsForType name fragments) =
    "Multiple inline fragments found for " <> show name <> ": " <> show fragments

-- | Object field separation operator.
--
-- Use this to provide handlers for fields of an object.
--
-- Say you had the following GraphQL type with \"foo\" and \"bar\" fields,
-- e.g.
--
-- @
--   type MyObject {
--     foo: Int!
--     bar: String!
--   }
-- @
--
-- You could provide handlers for it like this:
--
-- >>> :m +System.Environment
-- >>> let fooHandler = pure 42
-- >>> let barHandler = System.Environment.getProgName
-- >>> let myObjectHandler = pure $ fooHandler :<> barHandler :<> ()
data a :<> b = a :<> b
infixr 8 :<>


-- Result collects errors and values at the same time unless a handler
-- tells us to bail out in which case we stop the processing
-- immediately.
data Result a = Result [ResolverError] a deriving (Show, Functor, Eq)

-- Aggregating results keeps all errors and creates a ValueList
-- containing the individual values.
aggregateResults :: [Result Value] -> Result Value
aggregateResults r = toValue <$> sequenceA r

instance Applicative Result where
  pure v = Result [] v
  (Result e1 f) <*> (Result e2 x) = Result (e1 <> e2) (f x)

ok :: Value -> Result Value
ok = pure


class HasGraph m a where
  type Handler m a
  buildResolver :: Handler m a -> SelectionSet Value -> m (Result Value)

-- | Specify a default value for a type in a GraphQL schema.
--
-- GraphQL schema can have default values in certain places. For example,
-- arguments to fields can have default values. Because we cannot lift
-- arbitrary values to the type level, we need some way of getting at those
-- values. This typeclass provides the means.
--
-- To specify a default, implement this typeclass.
--
-- The default implementation is to say that there *is* no default for this
-- type.
class Defaultable a where
  -- | defaultFor returns the value to be used when no value has been given.
  defaultFor :: Name -> Maybe a
  defaultFor _ = empty

-- | Called when the schema expects an input argument @name@ of type @a@ but
-- @name@ has not been provided.
valueMissing :: Defaultable a => Name -> Either ResolverError a
valueMissing name = maybe (Left (ValueMissing name)) Right (defaultFor name)

instance Defaultable Int32

instance Defaultable Double

instance Defaultable Bool

instance Defaultable Text

instance Defaultable (Maybe a) where
  -- | The default for @Maybe a@ is @Nothing@.
  defaultFor _ = pure Nothing

instance forall m. (Functor m) => HasGraph m Int32 where
  type Handler m Int32 = m Int32
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  do
    map (ok . toValue) handler


instance forall m. (Functor m) => HasGraph m Double where
  type Handler m Double = m Double
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map (ok . toValue) handler

instance forall m. (Functor m) => HasGraph m Text where
  type Handler m Text = m Text
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map (ok . toValue) handler

instance forall m. (Functor m) => HasGraph m Bool where
  type Handler m Bool = m Bool
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map (ok . toValue) handler

instance forall m hg. (HasGraph m hg, Functor m, ToValue (Maybe hg)) => HasGraph m (Maybe hg) where
  type Handler m (Maybe hg) = m (Maybe hg)
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map (ok . toValue) handler


instance forall m hg. (Monad m, Applicative m, HasGraph m hg) => HasGraph m (API.List hg) where
  type Handler m (API.List hg) = m [Handler m hg]
  buildResolver handler selectionSet = do
    h <- handler
    let a = traverse (flip (buildResolver @m @hg) selectionSet) h
    map aggregateResults a


instance forall m ksN enum. (Applicative m, API.GraphQLEnum enum) => HasGraph m (API.Enum ksN enum) where
  type Handler m (API.Enum ksN enum) = enum
  buildResolver handler _ = (pure . ok . GValue.ValueEnum . API.enumToValue) handler


-- TODO: variables should error, they should have been resolved already.
--
-- TODO: Objects. Maybe implement some Generic object reader? I.e. if I do
-- data Greet = Greet { name :: Text, score :: Int } deriving Generic
-- then "instance ReadValue Greet" would fall back on a default reader that
-- expects Objects?
-- Maybe we can use advanced fallbacks like these:
-- https://wiki.haskell.org/GHC/AdvancedOverlap

-- | Internal data type to capture a field's name + what to execute if
-- the name matches the query. Note that the name is *not* in monad m,
-- but the value is. This is necessary so we can skip execution if the
-- name doesn't match.
data NamedValueResolver m = NamedValueResolver Name (m (Result Value))

-- Iterate through handlers (zipped together with their type
-- definition) and execute handler if the name matches.
type ResolveFieldResult = Result (Maybe GValue.ObjectField)

resolveField :: forall resolverType (m :: Type -> Type). (BuildFieldResolver m resolverType, Monad m)
  => FieldHandler m resolverType -> m ResolveFieldResult -> Field Value -> m ResolveFieldResult
resolveField handler nextHandler field =
  case buildFieldResolver @m @resolverType handler field of
    -- TODO the fact that this doesn't fit together nicely makes me think that ObjectField is not a good idea)
    Left err -> pure (Result [err] (Just (GValue.ObjectField queryFieldName GValue.ValueNull)))
    Right (NamedValueResolver name' resolver) -> runResolver name' resolver
  where
    runResolver :: Name -> m (Result Value) -> m ResolveFieldResult
    runResolver name' resolver
      | queryFieldName == name' = do
          Result errs value <- resolver
          pure (Result errs (Just (GValue.ObjectField queryFieldName value)))
      | otherwise = nextHandler
    queryFieldName = getName field

-- We're using our usual trick of rewriting a type in a closed type
-- family to emulate a closed typeclass. The following are the
-- universe of "allowed" class instances for field types:
data JustHandler a
data EnumField a b
data PlainField a b

-- injective helps with errors sometimes
type family FieldResolverDispatchType (a :: Type) = (r :: Type) | r -> a where
  FieldResolverDispatchType (API.Field ksA t) = JustHandler (API.Field ksA t)
  FieldResolverDispatchType (API.Argument ksB (API.Enum name t) :> f) = EnumField (API.Argument ksB (API.Enum name t)) (FieldResolverDispatchType f)
  FieldResolverDispatchType (API.Argument ksC t :> f) = PlainField (API.Argument ksC t) (FieldResolverDispatchType f)

-- | Derive the handler type from the Field/Argument type in a closed
-- type family: We don't want anyone else to extend this ever.
type family FieldHandler (m :: Type -> Type) (a :: Type) = (r :: Type) where
  FieldHandler m (JustHandler (API.Field ksD t)) = Handler m t
  FieldHandler m (PlainField (API.Argument ksE t) f) = t -> FieldHandler m f
  FieldHandler m (EnumField (API.Argument ksF (API.Enum name t)) f) = t -> FieldHandler m f

class BuildFieldResolver m fieldResolverType where
  buildFieldResolver :: FieldHandler m fieldResolverType -> Field Value -> Either ResolverError (NamedValueResolver m)

instance forall ksG t m.
  ( KnownSymbol ksG, HasGraph m t, HasAnnotatedType t, Monad m
  ) => BuildFieldResolver m (JustHandler (API.Field ksG t)) where
  buildFieldResolver handler field = do
    let resolver = buildResolver @m @t handler (getFieldSelectionSet field)
    field' <- first SchemaError (API.getFieldDefinition @(API.Field ksG t))
    let name = getName field'
    Right (NamedValueResolver name resolver)

instance forall ksH t f m name.
  ( KnownSymbol ksH
  , BuildFieldResolver m f
  , FromValue t
  , Defaultable t
  , HasAnnotatedInputType t
  , Monad m
  ) => BuildFieldResolver m (PlainField (API.Argument ksH t) f) where
  buildFieldResolver handler field = do
    argument <- first SchemaError (API.getArgumentDefinition @(API.Argument ksH t))
    let argName = getName argument
    value <- case lookupArgument field argName of
      Nothing -> valueMissing @t argName
      Just v -> first (InvalidValue argName) (fromValue @t v)
    buildFieldResolver @m @f (handler value) field

instance forall ksK t f m name.
  ( KnownSymbol ksK
  , BuildFieldResolver m f
  , KnownSymbol name
  , Defaultable t
  , API.GraphQLEnum t
  , Monad m
  ) => BuildFieldResolver m (EnumField (API.Argument ksK (API.Enum name t)) f) where
  buildFieldResolver handler field = do
    argName <- first SchemaError (AST.nameFromSymbol @ksK)
    value <- case lookupArgument field argName of
      Nothing -> panic "No"
      -- TODO extract enum name from v and ise instead of argName!
      Just v -> first (InvalidValue argName) (API.enumFromValue @t argName)
    buildFieldResolver @m @f (handler value) field

-- Note that we enumerate all ks variables with capital letters so we
-- can figure out error messages like the following that don't come
-- with line numbers:
--
--        • No instance for (GHC.TypeLits.KnownSymbol ks0)
--            arising from a use of ‘interpretAnonymousQuery’

-- We only allow Field and Argument :> Field combinations:
type family RunFieldsType (m :: Type -> Type) (a :: [Type]) = (r :: Type) where
  RunFieldsType m '[API.Field ksI t] = API.Field ksI t
  RunFieldsType m '[a :> b] = a :> b
  RunFieldsType m ((API.Field ksJ t) ': rest) = API.Field ksJ t :<> RunFieldsType m rest
  RunFieldsType m ((a :> b) ': rest) = (a :> b) :<> RunFieldsType m rest
  RunFieldsType m a = TypeError (
    'Text "All field entries in an Object must be Field or Argument :> Field. Got: " ':<>: 'ShowType a)

-- Match the three possible cases for Fields (see also RunFieldsType)
type family RunFieldsHandler (m :: Type -> Type) (a :: Type) = (r :: Type) where
  RunFieldsHandler m (f :<> fs) = FieldHandler m (FieldResolverDispatchType f) :<> RunFieldsHandler m fs
  RunFieldsHandler m (API.Field ksL t) = FieldHandler m (FieldResolverDispatchType (API.Field ksL t))
  RunFieldsHandler m (a :> b) = FieldHandler m (FieldResolverDispatchType (a :> b))
  RunFieldsHandler m a = TypeError (
    'Text "Unexpected RunFieldsHandler types: " ':<>: 'ShowType a)


class RunFields m a where
  -- runFields runs a a single AST.Selection over all possible fields
  -- (as specified by the type), returing exactly one ObjectField when
  -- a field matches, or an error otherwise.
  runFields :: RunFieldsHandler m a -> Field Value -> m ResolveFieldResult

instance forall f fs m dispatchType.
         ( BuildFieldResolver m dispatchType
         , dispatchType ~ FieldResolverDispatchType f
         , RunFields m fs
         , Monad m
         ) => RunFields m (f :<> fs) where
  runFields (handler :<> nextHandlers) selection =
    resolveField @dispatchType @m handler nextHandler selection
    where
      nextHandler = runFields @m @fs nextHandlers selection

instance forall ksM t m dispatchType.
         ( BuildFieldResolver m dispatchType
         , KnownSymbol ksM
         , dispatchType ~ FieldResolverDispatchType (API.Field ksM t)
         , Monad m
         ) => RunFields m (API.Field ksM t) where
  runFields handler field =
    resolveField @dispatchType @m handler nextHandler field
    where
      nextHandler = pure (Result [FieldNotFoundError field] Nothing)

instance forall m a b dispatchType.
         ( BuildFieldResolver m dispatchType
         , dispatchType ~ FieldResolverDispatchType (a :> b)
         , Monad m
         ) => RunFields m (a :> b) where
  runFields handler field =
    resolveField @dispatchType @m handler nextHandler field
    where
      nextHandler = pure (Result [FieldNotFoundError field] Nothing)

instance forall typeName interfaces fields m.
         ( RunFields m (RunFieldsType m fields)
         , Monad m
         ) => HasGraph m (API.Object typeName interfaces fields) where
  type Handler m (API.Object typeName interfaces fields) = m (RunFieldsHandler m (RunFieldsType m fields))

  buildResolver mHandler selectionSet = do
    -- First we run the actual handler function itself in IO.
    handler <- mHandler
    let fields = getFields selectionSet
    -- We're evaluating an Object so we're collecting ObjectFields from
    -- runFields and build a GValue.Map with them.
    r <- forM fields (runFields @m @(RunFieldsType m fields) handler)
    -- let (errs, fields) = foldr' (\(Result ea fa) (eb, fbs) -> (eb <> ea, fa:fbs)) ([], []) r
    let (Result errs obj)  = GValue.makeObject . catMaybes <$> sequenceA r
    case obj of
      Nothing -> pure (Result [DuplicateFields r] GValue.ValueNull)
      Just object -> pure (Result errs (GValue.ValueObject object))


-- TODO(tom): we're getting to a point where it might make sense to
-- split resolver into submodules (GraphQL.Resolver.Union  etc.)


-- | For unions we need a way to have type-safe, open sum types based
-- on the possible 'API.Object's of a union. The following closed type
-- family selects one Object from the union and returns the matching
-- 'HasGraph' 'Handler' type. If the object @o@ is not a member of
-- 'API.Union' then the user code won't compile.
--
-- This type family is an implementation detail but its TypeError
-- messages are visible at compile time.
type family TypeIndex (m :: Type -> Type) (object :: Type) (union :: Type) = (result :: Type) where
  TypeIndex m (API.Object name interfaces fields) (API.Union uName (API.Object name interfaces fields:_)) =
    Handler m (API.Object name interfaces fields)
  TypeIndex m (API.Object name interfaces fields) (API.Union uName (API.Object name' i' f':objects)) =
    TypeIndex m (API.Object name interfaces fields) (API.Union uName objects)
  -- Slightly nicer type errors:
  TypeIndex _ (API.Object name interfaces fields) (API.Union uName '[]) =
    TypeError ('Text "Type not found in union definition: " ':<>: 'ShowType (API.Object name interfaces fields))
  TypeIndex _ (API.Object name interfaces fields) x =
    TypeError ('Text "3rd type must be a union but it is: " ':<>: 'ShowType x)
  TypeIndex _ o _ =
    TypeError ('Text "Invalid TypeIndex. Must be Object but got: " ':<>: 'ShowType o)


-- | The 'Handler' type of a 'API.Union' must be the same for all
-- possible Objects, but each Object has a different type. We
-- unsafeCoerce the return type into an Any, tagging it with the union
-- and the underlying monad for type safety, but we elide the Object
-- type itself. This way we can represent all 'Handler' types of the
-- Union with a single type and still stay type-safe.
type role DynamicUnionValue representational representational
data DynamicUnionValue (union :: Type) (m :: Type -> Type) = DynamicUnionValue { _label :: Text, _value :: GHC.Exts.Any }

class RunUnion m union objects where
  runUnion :: DynamicUnionValue union m -> InlineFragment FragmentSpread Value -> m (Result Value)

instance forall m union objects name interfaces fields.
  ( Monad m
  , KnownSymbol name
  , TypeIndex m (API.Object name interfaces fields) union ~ Handler m (API.Object name interfaces fields)
  , RunFields m (RunFieldsType m fields)
  , RunUnion m union objects
  ) => RunUnion m union (API.Object name interfaces fields:objects) where
  runUnion duv fragment@(InlineFragment _ _ selection) =
    case extractUnionValue @(API.Object name interfaces fields) @union @m duv of
      Just handler -> buildResolver @m @(API.Object name interfaces fields) handler selection
      Nothing -> runUnion @m @union @objects duv fragment

-- AFAICT it should not be possible to ever hit the empty case because
-- the compiler doesn't allow constructing a unionValue that's not in
-- the Union. If the following code ever gets executed it's almost
-- certainly a bug in the union code.
--
-- We still need to implement this instance for the compiler because
-- it exhaustively checks all cases when deconstructs the Union.
instance forall m union. RunUnion m union '[] where
  runUnion (DynamicUnionValue label _) selection =
    panic ("Unexpected branch in runUnion, got " <> show selection <> " for label " <> label <> ". Please file a bug.")

instance forall m unionName objects.
  ( Monad m
  , KnownSymbol unionName
  , RunUnion m (API.Union unionName objects) objects
  ) => HasGraph m (API.Union unionName objects) where
  type Handler m (API.Union unionName objects) = m (DynamicUnionValue (API.Union unionName objects) m)
  -- 'label' is the name of the GraphQL type of the branch of the union that
  -- we are currently implementing.
  buildResolver mHandler selectionSet = do
    duv@(DynamicUnionValue label _) <- mHandler
    case AST.makeName label of
      Left e -> pure (Result [SchemaError e] GValue.ValueNull)
      Right name ->
        -- we only need to look at the fragment that matches by name:
        case findInlineFragmentForType name selectionSet of
          Left e -> pure (Result [e] GValue.ValueNull)
          Right inlineFragment -> do
            -- loop through union handlers and call right one when type matches.
            runUnion @m @(API.Union unionName objects) @objects duv inlineFragment

-- | Inline fragments have optional[*] type conditions. Find the inline
-- fragment in the selection set that matches the named type.
--
-- <https://facebook.github.io/graphql/#sec-Inline-Fragments>
--
-- [*] Except we currently treat type conditions as mandatory. This is a bug.
-- See <https://github.com/jml/graphql-api/issues/70>
--
-- Note: probably want to move this to Validation, esp. as part of work to
-- validate selection sets (see https://github.com/jml/graphql-api/issues/59).
findInlineFragmentForType :: Name -> SelectionSet Value -> Either ResolverError (InlineFragment FragmentSpread Value)
findInlineFragmentForType name selectionSet =
  case mapMaybe getInlineFragment selectionSet of
    [] -> Left (UnionTypeNotFound name selectionSet)
    [x] -> Right x
    xs -> Left (MultipleInlineFragmentsForType name xs)
  where
    getInlineFragment (SelectionInlineFragment frag@(InlineFragment (AST.NamedType name') _ _))
      | name == name' = Just frag
      | otherwise = Nothing
    getInlineFragment _ = Nothing

symbolText :: forall ks. KnownSymbol ks => Text
symbolText = toS (symbolVal @ks Proxy)

-- | Translate a 'Handler' into a DynamicUnionValue type required by
-- 'Union' handlers. This is dynamic, but nevertheless type-safe
-- because we can only tag with types that are part of the union.
--
-- Use e.g. like "unionValue @Cat" if you have an object like this:
--
-- >>> type Cat = API.Object "Cat" '[] '[API.Field "name" Text]
--
-- and then use `unionValue @Cat (pure (pure "Felix"))`. See
-- `examples/UnionExample.hs` for more code.
unionValue ::
  forall (object :: Type) (union :: Type) m (name :: Symbol) interfaces fields.
  (Monad m, API.Object name interfaces fields ~ object, KnownSymbol name)
  => TypeIndex m object union -> m (DynamicUnionValue union m)
unionValue x =
  -- TODO(tom) - we might want to move to Typeable `cast` for uValue
  -- instead of doing our own unsafeCoerce because it comes with
  -- additional safety guarantees: Typerep is unforgeable, while we
  -- can still into a bad place by matching on name only. We can't
  -- actually segfault this because right now we walk the list of
  -- objects in a union left-to-right so in case of duplicate names we
  -- only every see one type. That doesn't seen like a great thing to
  -- rely on though!

  -- Note that unsafeCoerce is safe because we index the type from the
  -- union with an 'API.Object' whose name we're storing in label. On
  -- the way out we check that the name is the same, and we know the
  -- type universe is the same because we annotated DynamicUnionValue
  -- with the type universe.
  pure (DynamicUnionValue (symbolText @name) (unsafeCoerce x))

extractUnionValue ::
  forall (object :: Type) (union :: Type) m (name :: Symbol) interfaces fields.
  (API.Object name interfaces fields ~ object, KnownSymbol name)
  => DynamicUnionValue union m -> Maybe (TypeIndex m object union)
extractUnionValue (DynamicUnionValue uName uValue) =
  if uName == symbolText @name
  then Just (unsafeCoerce uValue)
  else Nothing
