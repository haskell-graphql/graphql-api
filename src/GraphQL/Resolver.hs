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

module GraphQL.Resolver
  ( ResolverError(..) -- XXX: Exporting constructor for tests. Not sure if that's what we really want.
  , HasGraph(..)
  , (:<>)(..)
  , (:<|>)(..)
  , BuildFieldResolver(..)
  , Result(..)
  ) where

-- TODO (probably incomplete, the spec is large)
-- - input objects - I'm not super clear from the spec on how
--   they differ from normal objects.
-- - "extend type X" is used in examples in the spec but it's not
--   explained anywhere?
-- - Directives (https://facebook.github.io/graphql/#sec-Type-System.Directives)
-- - Enforce non-empty lists (might only be doable via value-level validation)

import Protolude hiding (Enum)
import GHC.TypeLits (KnownSymbol)
import qualified GHC.TypeLits as TypeLits

import GraphQL.API
  ( (:>)
  , HasAnnotatedType(..)
  , HasAnnotatedInputType(..)
  )
import qualified GraphQL.API as API
import qualified GraphQL.Value as GValue
import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.Schema (HasName(..))
import GraphQL.Internal.Input (CanonicalQuery)

data ResolverError
  -- | There was a problem in the schema. Server-side problem.
  = SchemaError AST.NameError
  -- | Couldn't find the requested field in the object. A client-side problem.
  | FieldNotFoundError AST.Selection
  -- | No value provided for name, and no default specified. Client-side problem.
  | ValueMissing AST.Name
  -- | Could not translate value into Haskell. Probably a client-side problem.
  | InvalidValue AST.Name Text
  -- | Found duplicate fields in set.
  | DuplicateFields [ResolveFieldResult]
  -- | We tried to resolve something that wasn't a field. Once our validation
  -- stuff is sorted out this error should go away.
  | ResolveNonField AST.Selection
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

-- | Union type separation operator.
data a :<|> b = a :<|> b
infixr 8 :<|>


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
  -- TODO it's not obvious to me that this is the right function...
  (Result e1 f) <*> (Result e2 x) = Result (e1 <> e2) (f x)

ok :: GValue.Value -> Result GValue.Value
ok = pure


-- TODO instead of SelectionSet we want something like
-- NormalizedSelectionSet which has query fragments etc. resolved.
class HasGraph m a where
  type Handler m a
  buildResolver :: Handler m a -> CanonicalQuery -> m (Result GValue.Value)


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

-- TODO not super hot on individual values having to be instances of
-- HasGraph but not sure how else we can nest either types or
-- (Object _ _ fields). Maybe instead of field we need a "SubObject"?
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
  buildResolver handler _ = (pure . ok . API.enumToValue) handler


-- TODO: lookup is O(N^2) in number of arguments (we linearly search
-- each argument in the list) but considering the graphql use case
-- where N usually < 10 this is probably OK.
lookupValue :: AST.Name -> [AST.Argument] -> Maybe GValue.Value
lookupValue name args = case find (\(AST.Argument name' _) -> name' == name) args of
  Nothing -> Nothing
  Just (AST.Argument _ value) -> GValue.astToValue value


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
  => FieldHandler m a -> m ResolveFieldResult -> AST.Selection -> m ResolveFieldResult
resolveField handler nextHandler selection@(AST.SelectionField (AST.Field _ queryFieldName _ _ _)) =
  case buildFieldResolver @m @a handler selection of
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
resolveField _ _ f =
  pure (Result [ResolveNonField f] Nothing)


-- | Derive the handler type from the Field/Argument type in a closed
-- type family: We don't want anyone else to extend this ever.
type family FieldHandler m (a :: Type) :: Type where
  FieldHandler m (API.Field ks t) = Handler m t
  FieldHandler m (API.Argument ks t :> f) = t -> FieldHandler m f

class BuildFieldResolver m a where
  buildFieldResolver :: FieldHandler m a -> AST.Selection -> Either ResolverError (NamedValueResolver m)

instance forall ks t m. (KnownSymbol ks, HasGraph m t, HasAnnotatedType t, Monad m) => BuildFieldResolver m (API.Field ks t) where
  buildFieldResolver handler (AST.SelectionField (AST.Field _ _ _ _ selectionSet)) = do
    let resolver = buildResolver @m @t handler selectionSet
    field <- first SchemaError (API.getFieldDefinition @(API.Field ks t))
    let name = getName field
    Right (NamedValueResolver name resolver)
  buildFieldResolver _ f = Left (ResolveNonField f)


instance forall ks t f m.
  ( KnownSymbol ks
  , BuildFieldResolver m f
  , GValue.FromValue t
  , Defaultable t
  , HasAnnotatedInputType t
  , Monad m
  ) => BuildFieldResolver m (API.Argument ks t :> f) where
  buildFieldResolver handler selection@(AST.SelectionField (AST.Field _ _ arguments _ _)) = do
    argument <- first SchemaError (API.getArgumentDefinition @(API.Argument ks t))
    let argName = getName argument
    value <- case lookupValue argName arguments of
      Nothing -> valueMissing @t argName
      Just v -> first (InvalidValue argName) (GValue.fromValue @t v)
    buildFieldResolver @m @f (handler value) selection
  buildFieldResolver _ f = Left (ResolveNonField f)


-- We only allow Field and Argument :> Field combinations:
type family RunFieldsType (m :: Type -> Type) (a :: [Type]) = (r :: Type) where
  RunFieldsType m '[API.Field ks t] = API.Field ks t
  RunFieldsType m '[a :> b] = a :> b
  RunFieldsType m ((API.Field ks t) ': rest) = API.Field ks t :<> RunFieldsType m rest
  RunFieldsType m ((a :> b) ': rest) = (a :> b) :<> RunFieldsType m rest
  RunFieldsType m a = TypeLits.TypeError (
    'TypeLits.Text "All field entries in an Object must be Field or Argument :> Field. Got: " 'TypeLits.:<>: 'TypeLits.ShowType a)

-- Match the three possible cases for Fields (see also RunFieldsType)
type family RunFieldsHandler (m :: Type -> Type) (a :: Type) = (r :: Type) where
  RunFieldsHandler m (f :<> fs) = FieldHandler m f :<> RunFieldsHandler m fs
  RunFieldsHandler m (API.Field ks t) = FieldHandler m (API.Field ks t)
  RunFieldsHandler m (a :> b) = FieldHandler m (a :> b)
  RunFieldsHandler m a = TypeLits.TypeError (
    'TypeLits.Text "Unexpected RunFieldsHandler types: " 'TypeLits.:<>: 'TypeLits.ShowType a)


class RunFields m a where
  -- runFields is run on a single AST.Selection so it can only ever
  -- return one ObjectField.
  runFields :: RunFieldsHandler m a -> AST.Selection -> m ResolveFieldResult

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
  runFields handler selection =
    resolveField @(API.Field ks t) @m handler nextHandler selection
    where
      nextHandler = pure (Result [FieldNotFoundError selection] Nothing)

instance forall m a b.
         ( BuildFieldResolver m (a :> b)
         , Monad m
         ) => RunFields m (a :> b) where
  runFields handler selection =
    resolveField @(a :> b) @m handler nextHandler selection
    where
      nextHandler = pure (Result [FieldNotFoundError selection] Nothing)


instance forall typeName interfaces fields m.
         ( RunFields m (RunFieldsType m fields)
         , Monad m
         ) => HasGraph m (API.Object typeName interfaces fields) where
  type Handler m (API.Object typeName interfaces fields) = m (RunFieldsHandler m (RunFieldsType m fields))

  buildResolver mHandler selectionSet = do
    -- First we run the actual handler function itself in IO.
    handler <- mHandler
    -- We're evaluating an Object so we're collecting ObjectFields from
    -- runFields and build a GValue.Map with them.
    r <- forM selectionSet (runFields @m @(RunFieldsType m fields) handler)
    -- let (errs, fields) = foldr' (\(Result ea fa) (eb, fbs) -> (eb <> ea, fa:fbs)) ([], []) r
    let (Result errs obj)  = GValue.makeObject . catMaybes <$> sequenceA r
    case obj of
      Nothing -> pure (Result [DuplicateFields r] GValue.ValueNull)
      Just object -> pure (Result errs (GValue.ValueObject object))
