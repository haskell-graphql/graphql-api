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

module GraphQL.Server
  ( QueryError(..) -- XXX: Exporting constructor for tests. Not sure if that's what we really want.
  , HasGraph(..)
  , (:<>)(..)
  , (:<|>)(..)
  , BuildFieldResolver(..)
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

import qualified GraphQL.Value as GValue
import qualified GraphQL.Internal.AST as AST
import GraphQL.Internal.Schema (HasName(..))
-- TODO: Explicit import
import GraphQL.API
import GraphQL.Internal.Input (CanonicalQuery)

import Control.Monad.Catch (MonadThrow, throwM, Exception)

-- | MonadThrow requires an instance of Exception so we create a
-- newtype for GraphQL errors.
newtype QueryError = QueryError Text deriving (Show, Eq)
instance Exception QueryError

-- TODO: throwM throws in the base monad, and that's often IO. If we
-- want to support PartialSuccess we need a different error model to
-- throwM.
queryError :: forall m a. MonadThrow m => Text -> m a
queryError = throwM . QueryError

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

-- TODO instead of SelectionSet we want something like
-- NormalizedSelectionSet which has query fragments etc. resolved.
class (MonadThrow m, MonadIO m) => HasGraph m a where
  type Handler m a
  buildResolver :: Handler m a -> CanonicalQuery -> m GValue.Value


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
valueMissing :: Defaultable a => AST.Name -> Either Text a
valueMissing name = maybe (Left ("Value missing: " <> AST.getNameText name)) Right (defaultFor name)

instance Defaultable Int32

instance Defaultable Double

instance Defaultable Bool

instance Defaultable Text

-- TODO not super hot on individual values having to be instances of
-- HasGraph but not sure how else we can nest either types or
-- (Object _ _ fields). Maybe instead of field we need a "SubObject"?
instance forall m. (MonadThrow m, MonadIO m) => HasGraph m Int32 where
  type Handler m Int32 = m Int32
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map GValue.toValue handler

instance forall m. (MonadThrow m, MonadIO m) => HasGraph m Double where
  type Handler m Double = m Double
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map GValue.toValue handler

instance forall m. (MonadThrow m, MonadIO m) => HasGraph m Text where
  type Handler m Text = m Text
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map GValue.toValue handler


instance forall m hg. (MonadThrow m, MonadIO m, HasGraph m hg) => HasGraph m (List hg) where
  type Handler m (List hg) = [Handler m hg]
  buildResolver handler selectionSet =
    let a = traverse (flip (buildResolver @m @hg) selectionSet) handler
    in map GValue.toValue a

instance forall m ks enum. (MonadThrow m, MonadIO m, GraphQLEnum enum) => HasGraph m (Enum ks enum) where
  type Handler m (Enum ks enum) = enum
  buildResolver handler _ = pure (enumToValue handler)


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
data NamedFieldExecutor m = NamedFieldExecutor AST.Name (m GValue.Value)

executeNamedField :: Monad m => AST.Field -> NamedFieldExecutor m -> m (Maybe GValue.ObjectField)
executeNamedField (AST.Field alias name _ _ _) (NamedFieldExecutor k mValue)
  | name == k = do
      value <- mValue
      let name' = fromMaybe name alias
      pure (Just (GValue.ObjectField name' value))
  | otherwise = pure Nothing

-- Deconstruct object type signature and handler value at the same
-- time and run type-directed code for each field.
resolveField :: forall a (m :: Type -> Type). BuildFieldResolver m a => m GValue.ObjectField -> FieldHandler m a -> AST.Selection -> m GValue.ObjectField
resolveField fallback lh selection@(AST.SelectionField field) = do
  let fieldExecutor = buildFieldResolver @m @a lh selection
  case fieldExecutor of
    Left err -> throwM err
    Right executor -> do
      objectField <- executeNamedField field executor
      maybe fallback pure objectField
resolveField _ _ f = queryError ("Unexpected Selection value. Is the query normalized?: " <> show f)


-- | Derive the handler type from the Field/Argument type in a closed
-- type family: We don't want anyone else to extend this ever.
type family FieldHandler m (a :: Type) :: Type where
  FieldHandler m (Field ks t) = Handler m t
  FieldHandler m (Argument ks t :> f) = t -> FieldHandler m f

class (MonadThrow m, MonadIO m) => BuildFieldResolver m a where
  buildFieldResolver :: FieldHandler m a -> AST.Selection -> Either QueryError (NamedFieldExecutor m)

instance forall ks t m. (KnownSymbol ks, HasGraph m t, HasAnnotatedType t, MonadThrow m, MonadIO m) => BuildFieldResolver m (Field ks t) where
  buildFieldResolver handler (AST.SelectionField (AST.Field _ _ _ _ selectionSet)) = do
    let childResolver = buildResolver @m @t handler selectionSet
    field <- first (QueryError. AST.formatNameError) (getFieldDefinition @(Field ks t))
    let name = getName field
    pure (NamedFieldExecutor name childResolver)
  buildFieldResolver _ f =
    Left (QueryError ("buildFieldResolver got non AST.Field" <> show f <> ", query probably not normalized"))


instance forall ks t f m.
         ( MonadThrow m
         , KnownSymbol ks
         , BuildFieldResolver m f
         , GValue.FromValue t
         , Defaultable t
         , HasAnnotatedInputType t
         ) => BuildFieldResolver m (Argument ks t :> f) where
  buildFieldResolver handler selection@(AST.SelectionField (AST.Field _ _ arguments _ _)) = do
    argument <- first (QueryError . AST.formatNameError) (getArgumentDefinition @(Argument ks t))
    let argName = getName argument
    value <- first QueryError (maybe (valueMissing @t argName) (GValue.fromValue @t) (lookupValue argName arguments))
    buildFieldResolver @m @f (handler value) selection
  buildFieldResolver _ f =
    Left (QueryError ("buildFieldResolver got non AST.Field" <> show f <> ", query probably not normalized"))


-- We only allow Field and Argument :> Field combinations:
type family RunFieldsType (m :: Type -> Type) (a :: [Type]) = (r :: Type) where
  RunFieldsType m '[Field ks t] = Field ks t
  RunFieldsType m '[a :> b] = a :> b
  RunFieldsType m ((Field ks t) ': rest) = Field ks t :<> RunFieldsType m rest
  RunFieldsType m ((a :> b) ': rest) = (a :> b) :<> RunFieldsType m rest
  RunFieldsType m a = TypeLits.TypeError (
    'TypeLits.Text "All field entries in an Object must be Field or Argument :> Field. Got: " 'TypeLits.:<>: 'TypeLits.ShowType a)


class RunFields m a where
  type RunFieldsHandler m a :: Type
  -- runFields is run on a single QueryTerm so it can only ever return
  -- one ObjectField.
  runFields :: RunFieldsHandler m a -> AST.Selection -> m GValue.ObjectField


instance forall f fs m.
         ( BuildFieldResolver m f
         , RunFields m fs
         , MonadThrow m
         , MonadIO m
         ) => RunFields m (f :<> fs) where
  type RunFieldsHandler m (f :<> fs) = FieldHandler m f :<> RunFieldsHandler m fs
  runFields (lh :<> rh) selection =
    resolveField @f @m fallback lh selection
    where
      fallback = runFields @m @fs rh selection

instance forall ks t m.
         ( BuildFieldResolver m (Field ks t)
         , MonadThrow m
         , MonadIO m
         ) => RunFields m (Field ks t) where
  type RunFieldsHandler m (Field ks t) = FieldHandler m (Field ks t)
  runFields lh selection =
    resolveField @(Field ks t) @m fallback lh selection
    where
      fallback = queryError ("Query for undefined selection: " <> show selection)

instance forall m a b.
         ( BuildFieldResolver m (a :> b)
         , MonadThrow m
         , MonadIO m
         ) => RunFields m (a :> b) where
  type RunFieldsHandler m (a :> b) = FieldHandler m (a :> b)
  runFields lh selection =
    resolveField @(a :> b) @m fallback lh selection
    where
      fallback = queryError ("Query for undefined selection: " <> show selection)


instance forall typeName interfaces fields m.
         ( RunFields m (RunFieldsType m fields)
         , MonadThrow m
         , MonadIO m
         ) => HasGraph m (Object typeName interfaces fields) where
  type Handler m (Object typeName interfaces fields) = m (RunFieldsHandler m (RunFieldsType m fields))

  buildResolver mHandler selectionSet = do
    -- First we run the actual handler function itself in IO.
    handler <- mHandler
    -- We're evaluating an Object so we're collecting ObjectFields from
    -- runFields and build a GValue.Map with them.
    r <- forM selectionSet $ runFields @m @(RunFieldsType m fields) handler
    case GValue.makeObject r of
      Nothing -> queryError $ "Duplicate fields in set: " <> show r
      Just object -> pure $ GValue.ValueObject object


-- | Closed type family to enforce the invariant that Union types
-- contain only Objects.
type family RunUnionType m (a :: [Type]) :: Type where
  RunUnionType m '[Object typeName interfaces fields] = Object typeName interfaces fields
  RunUnionType m (Object typeName interfaces fields:rest) = Object typeName interfaces fields :<|> RunUnionType m rest
  RunUnionType m a = TypeLits.TypeError ('TypeLits.Text "All types in a union must be Object. Got: " 'TypeLits.:<>: 'TypeLits.ShowType a)

type family RunUnionHandlerType m (a :: Type) :: Type where
  RunUnionHandlerType m (o :<|> rest) =
    Handler m o :<|> RunUnionHandlerType m rest
  RunUnionHandlerType m o =
    Handler m o

-- Type class to execute union type queries.
class RunUnion m a where
  runUnion :: RunUnionHandlerType m a -> AST.Selection -> m GValue.Object

instance forall m object rest.
         ( RunUnion m rest
         , MonadIO m
         , MonadThrow m
         , HasGraph m object
         , HasObjectDefinition object
         ) => RunUnion m (object :<|> rest) where
  runUnion (lh :<|> rh) fragment@(AST.SelectionInlineFragment (AST.InlineFragment (AST.NamedType queryTypeName) [] subSelection)) =
    case getDefinition @object of
      Left err -> queryError (AST.formatNameError err)
      Right object ->
        if getName object == queryTypeName
        then do
          result <- buildResolver @m @object lh subSelection
          -- TODO: See if we can prevent this from happening at compile time.
          --
          -- We should definitely not be panicking in our library code because
          -- the user returned a Bool from their union type.
          case GValue.toObject result of
            Nothing -> panic $ "Expected object as result of union query: " <> show result
            Just obj -> pure obj
        else
          runUnion @m @rest rh fragment
  runUnion _ _ =
    queryError "Non-InlineFragment used for a union type query."

-- TODO(jml): I don't understand why I can't extract (Object typeName interfaces fields)
-- from this instance as I did for the above instance.
instance forall m typeName interfaces fields.
         ( MonadIO m
         , MonadThrow m
         , RunFields m (RunFieldsType m fields)
         , KnownSymbol typeName
         , HasObjectDefinition (Object typeName interfaces fields)
         ) => RunUnion m (Object typeName interfaces fields) where
  runUnion lh fragment@(AST.SelectionInlineFragment (AST.InlineFragment (AST.NamedType queryTypeName) [] subSelection)) =
    case getDefinition @(Object typeName interfaces fields) of
      Left err -> queryError (AST.formatNameError err)
      Right object ->
        if getName object == queryTypeName
        then do
          result <- buildResolver @m @(Object typeName interfaces fields) lh subSelection
          -- TODO: See if we can prevent this from happening at compile time.
          case GValue.toObject result of
            Nothing -> panic $ "Expected object as result of union query: " <> show result
            Just obj -> pure obj
        else
          queryError ("Union type could not be resolved:" <> show fragment)
  runUnion _ _ =
    queryError "Non-InlineFragment used for a union type query."

instance forall m ks ru.
         ( MonadThrow m
         , MonadIO m
         , RunUnion m (RunUnionType m ru)
         ) => HasGraph m (Union ks ru) where
  type Handler m (Union ks ru) = RunUnionHandlerType m (RunUnionType m ru)
  -- TODO: check sanity of query before executing it. E.g. we can't
  -- have the same field name in two different fragment branches
  -- (needs to take aliases into account).

  -- query "{ ... on Human { name } }"
  -- [SelectionInlineFragment (InlineFragment (NamedType "Human") [] [SelectionField (Field "" "name" [] [] [])])]
  buildResolver handler selection = do
    -- GraphQL invariant is that all items in a Union must be objects
    -- which means 1) they have fields 2) They are ValueMap
    values <- map GValue.unionObjects (traverse (runUnion @m @(RunUnionType m ru) handler) selection)
    maybe (panic $ "Duplicate fields in values: " <> show values) (pure . GValue.ValueObject) values
