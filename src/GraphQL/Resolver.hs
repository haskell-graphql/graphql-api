{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- for TypeError
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PatternSynonyms #-}

module GraphQL.Resolver
  ( ResolverError(..) -- XXX: Exporting constructor for tests. Not sure if that's what we really want.
  , HasGraph(..)
  , (:<>)(..)
  , BuildFieldResolver(..)
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
import qualified GraphQL.Internal.AST as AST
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
  | FieldNotFoundError (Field AST.Value)
  -- | No value provided for name, and no default specified. Client-side problem.
  | ValueMissing AST.Name
  -- | Could not translate value into Haskell. Probably a client-side problem.
  | InvalidValue AST.Name Text
  -- | Found duplicate fields in set.
  | DuplicateFields [ResolveFieldResult]
  -- | We found a variable in an input value. We should only have constants at
  -- this point.
  | UnresolvedVariable AST.Name AST.Value
  -- | We tried to resolve something that wasn't a field. Once our validation
  -- stuff is sorted out this error should go away.
  | ResolveNonField AST.Selection
  -- | We tried to resolve something that wasn't a union type despite
  -- expecting one.
  | ResolveNonUnionType Text (SelectionSet AST.Value)
  -- | We tried to use an inline fragment with a name that the union
  -- type does not support.
  | UnionTypeNotFound Text (SelectionSet AST.Value)
  deriving (Show, Eq)

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
aggregateResults :: [Result GValue.Value] -> Result GValue.Value
aggregateResults r = GValue.toValue <$> sequenceA r

instance Applicative Result where
  pure v = Result [] v
  (Result e1 f) <*> (Result e2 x) = Result (e1 <> e2) (f x)

ok :: GValue.Value -> Result GValue.Value
ok = pure


class HasGraph m a where
  type Handler m a
  buildResolver :: Handler m a -> SelectionSet AST.Value -> m (Result GValue.Value)

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
  defaultFor :: AST.Name -> Maybe a
  defaultFor _ = empty

-- | Called when the schema expects an input argument @name@ of type @a@ but
-- @name@ has not been provided.
valueMissing :: Defaultable a => AST.Name -> Either ResolverError a
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
    map (ok . GValue.toValue) handler


instance forall m. (Functor m) => HasGraph m Double where
  type Handler m Double = m Double
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map (ok . GValue.toValue) handler

instance forall m. (Functor m) => HasGraph m Text where
  type Handler m Text = m Text
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map (ok . GValue.toValue) handler


instance forall m hg. (Applicative m, HasGraph m hg) => HasGraph m (API.List hg) where
  type Handler m (API.List hg) = [Handler m hg]
  buildResolver handler selectionSet = map aggregateResults a
    where
      a = traverse (flip (buildResolver @m @hg) selectionSet) handler

instance forall m ks enum. (Applicative m, API.GraphQLEnum enum) => HasGraph m (API.Enum ks enum) where
  type Handler m (API.Enum ks enum) = enum
  buildResolver handler _ = (pure . ok . GValue.ValueEnum . API.enumToValue) handler


-- TODO: lookup is O(N log N) in number of arguments (we linearly search each
-- argument in the list) but considering the graphql use case where N usually
-- < 10 this is probably OK.
lookupValue :: AST.Name -> Field AST.Value -> Maybe GValue.Value
lookupValue name field = do
  ast <- lookupArgument field name
  variable <- GValue.astToVariableValue ast
  traverse hush variable

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
data NamedValueResolver m = NamedValueResolver AST.Name (m (Result GValue.Value))

-- Iterate through handlers (zipped together with their type
-- definition) and execute handler if the name matches.
type ResolveFieldResult = Result (Maybe GValue.ObjectField)

resolveField :: forall a (m :: Type -> Type). (BuildFieldResolver m a, Monad m)
  => FieldHandler m a -> m ResolveFieldResult -> Field AST.Value -> m ResolveFieldResult
resolveField handler nextHandler field =
  case buildFieldResolver @m @a handler field of
    -- TODO the fact that this doesn't fit together nicely makes me think that ObjectField is not a good idea)
    Left err -> pure (Result [err] (Just (GValue.ObjectField queryFieldName GValue.ValueNull)))
    Right (NamedValueResolver name' resolver) -> runResolver name' resolver
  where
    runResolver :: AST.Name -> m (Result GValue.Value) -> m ResolveFieldResult
    runResolver name' resolver
      | queryFieldName == name' = do
          Result errs value <- resolver
          pure (Result errs (Just (GValue.ObjectField queryFieldName value)))
      | otherwise = nextHandler
    queryFieldName = getName field

-- | Derive the handler type from the Field/Argument type in a closed
-- type family: We don't want anyone else to extend this ever.
type family FieldHandler m (a :: Type) :: Type where
  FieldHandler m (API.Field ks t) = Handler m t
  FieldHandler m (API.Argument ks t :> f) = t -> FieldHandler m f

class BuildFieldResolver m a where
  buildFieldResolver :: FieldHandler m a -> Field AST.Value -> Either ResolverError (NamedValueResolver m)

instance forall ks t m. (KnownSymbol ks, HasGraph m t, HasAnnotatedType t, Monad m) => BuildFieldResolver m (API.Field ks t) where
  buildFieldResolver handler field = do
    let resolver = buildResolver @m @t handler (getFieldSelectionSet field)
    field' <- first SchemaError (API.getFieldDefinition @(API.Field ks t))
    let name = getName field'
    Right (NamedValueResolver name resolver)


instance forall ks t f m.
  ( KnownSymbol ks
  , BuildFieldResolver m f
  , GValue.FromValue t
  , Defaultable t
  , HasAnnotatedInputType t
  , Monad m
  ) => BuildFieldResolver m (API.Argument ks t :> f) where
  buildFieldResolver handler field = do
    argument <- first SchemaError (API.getArgumentDefinition @(API.Argument ks t))
    let argName = getName argument
    value <- case lookupValue argName field of
      Nothing -> valueMissing @t argName
      Just v -> first (InvalidValue argName) (GValue.fromValue @t v)
    buildFieldResolver @m @f (handler value) field


-- We only allow Field and Argument :> Field combinations:
type family RunFieldsType (m :: Type -> Type) (a :: [Type]) = (r :: Type) where
  RunFieldsType m '[API.Field ks t] = API.Field ks t
  RunFieldsType m '[a :> b] = a :> b
  RunFieldsType m ((API.Field ks t) ': rest) = API.Field ks t :<> RunFieldsType m rest
  RunFieldsType m ((a :> b) ': rest) = (a :> b) :<> RunFieldsType m rest
  RunFieldsType m a = TypeError (
    'Text "All field entries in an Object must be Field or Argument :> Field. Got: " ':<>: 'ShowType a)

-- Match the three possible cases for Fields (see also RunFieldsType)
type family RunFieldsHandler (m :: Type -> Type) (a :: Type) = (r :: Type) where
  RunFieldsHandler m (f :<> fs) = FieldHandler m f :<> RunFieldsHandler m fs
  RunFieldsHandler m (API.Field ks t) = FieldHandler m (API.Field ks t)
  RunFieldsHandler m (a :> b) = FieldHandler m (a :> b)
  RunFieldsHandler m a = TypeError (
    'Text "Unexpected RunFieldsHandler types: " ':<>: 'ShowType a)


class RunFields m a where
  -- runFields is run on a single AST.Selection so it can only ever
  -- return one ObjectField.
  runFields :: RunFieldsHandler m a -> Field AST.Value -> m ResolveFieldResult

instance forall f fs m.
         ( BuildFieldResolver m f
         , RunFields m fs
         , Monad m
         ) => RunFields m (f :<> fs) where
  runFields (handler :<> nextHandlers) selection =
    resolveField @f @m handler nextHandler selection
    where
      nextHandler = runFields @m @fs nextHandlers selection

instance forall ks t m.
         ( BuildFieldResolver m (API.Field ks t)
         , Monad m
         ) => RunFields m (API.Field ks t) where
  runFields handler field =
    resolveField @(API.Field ks t) @m handler nextHandler field
    where
      nextHandler = pure (Result [FieldNotFoundError field] Nothing)

instance forall m a b.
         ( BuildFieldResolver m (a :> b)
         , Monad m
         ) => RunFields m (a :> b) where
  runFields handler field =
    resolveField @(a :> b) @m handler nextHandler field
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
  TypeIndex m (API.Object name interfaces fields) (API.Union uName ((API.Object name interfaces fields):_)) =
    Handler m (API.Object name interfaces fields)
  TypeIndex m (API.Object name interfaces fields) (API.Union uName ((API.Object name' i' f'):objects)) =
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
  runUnion :: DynamicUnionValue union m -> InlineFragment (FragmentSpread AST.Value) AST.Value -> m (Result GValue.Value)

instance forall m union objects name interfaces fields.
  ( Monad m
  , KnownSymbol name
  , TypeIndex m (API.Object name interfaces fields) union ~ Handler m (API.Object name interfaces fields)
  , RunFields m (RunFieldsType m fields)
  , RunUnion m union objects
  ) => RunUnion m union ((API.Object name interfaces fields):objects) where
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
  buildResolver mHandler selectionSet = do
    duv@(DynamicUnionValue label _) <- mHandler
    -- we only need to look at the fragment that matches by name:
    case find (matchFragmentName label) selectionSet of
      Nothing -> pure (Result [UnionTypeNotFound label selectionSet] GValue.ValueNull)
      Just (SelectionInlineFragment inlineFragment) -> do
        -- loop through union handlers and call right one when type matches.
        runUnion @m @(API.Union unionName objects) @objects duv inlineFragment
      Just _ -> pure (Result [ResolveNonUnionType label selectionSet] GValue.ValueNull)
      where
        matchFragmentName label' (SelectionInlineFragment (InlineFragment (AST.NamedType name') _ _)) =
          -- TODO: error handler
          label' == AST.getNameText name'
        matchFragmentName _ _ = False


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
