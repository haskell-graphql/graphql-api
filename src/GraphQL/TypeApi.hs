{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GraphQL.TypeApi
  ( QueryError
  , HasGraph(..)
  , ReadValue(..)
  , BuildFieldResolver(..)
  ) where

-- TODO (probably incomplete, the spec is large)
-- * input objects - I'm not super clear from the spec on how
--   they differ from normal objects.
-- * "extend type X" is used in examples in the spec but it's not
--   explained anywhere?
-- * Directives (https://facebook.github.io/graphql/#sec-Type-System.Directives)
-- * Enforce non-empty lists (might only be doable via value-level validation)

import Protolude hiding (Enum)
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified GraphQL.Value as GValue
import qualified Data.Map as M
import qualified Data.GraphQL.AST as AST
import GraphQL.Definitions
import GraphQL.Input (CanonicalQuery)

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


-- TODO instead of SelectionSet we want something like
-- NormalizedSelectionSet which has query fragments etc. resolved.
class (MonadThrow m, MonadIO m) => HasGraph m a where
  type HandlerType m a
  buildResolver :: HandlerType m a -> CanonicalQuery -> m GValue.Value

-- | The ReadValue instance converts AST.Value types like ValueInt to
-- the type expected by the handler function. It's the boundary
-- between incoming types and your custom application Haskell types.
class ReadValue a where
  -- | Convert the already-parsed value into the type needed for the
  -- function call.
  readValue :: AST.Value -> Either Text a

  -- | valueMissing is a separate function so we can provide default
  -- values for certain cases. E.g. there is an instance for `Maybe a`
  -- that returns Nothing if the value is missing.
  valueMissing :: AST.Name -> Either Text a
  valueMissing name' = Left ("Value missing: " <> name')


-- TODO not super hot on individual values having to be instances of
-- HasGraph but not sure how else we can nest either types or
-- (Object _ _ fields). Maybe instead of field we need a "SubObject"?
instance forall m. (MonadThrow m, MonadIO m) => HasGraph m Int32 where
  type HandlerType m Int32 = m Int32
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map GValue.toValue handler


instance forall m. (MonadThrow m, MonadIO m) => HasGraph m Double where
  type HandlerType m Double = m Double
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map GValue.toValue handler

instance forall m. (MonadThrow m, MonadIO m) => HasGraph m Text where
  type HandlerType m Text = m Text
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  map GValue.toValue handler


instance forall m hg. (MonadThrow m, MonadIO m, HasGraph m hg) => HasGraph m (List hg) where
  type HandlerType m (List hg) = [HandlerType m hg]
  buildResolver handler selectionSet =
    let a = traverse (flip (buildResolver @m @hg) selectionSet) handler
    in map GValue.toValue a

instance forall m ks enum. (MonadThrow m, MonadIO m, GraphQLEnum enum) => HasGraph m (Enum ks enum) where
  type HandlerType m (Enum ks enum) = enum
  buildResolver handler _ = pure (enumToValue handler)


-- TODO: lookup is O(N^2) in number of arguments (we linearly search
-- each argument in the list) but considering the graphql use case
-- where N usually < 10 this is probably OK.
lookupValue :: AST.Name -> [AST.Argument] -> Maybe AST.Value
lookupValue name args = case find (\(AST.Argument name' _) -> name' == name) args of
  Nothing -> Nothing
  Just (AST.Argument _ value) -> Just value


instance ReadValue Int32 where
  readValue (AST.ValueInt v) = pure v
  readValue v = Left ("Not an Int:" <> (show v))

-- TODO: Double parsing is broken in graphql-haskell.
-- See https://github.com/jdnavarro/graphql-haskell/pull/16
instance ReadValue Double where
  readValue (AST.ValueFloat v) = pure v
  readValue v = Left ("Not a Double:" <> (show v))

instance ReadValue Bool where
  readValue (AST.ValueBoolean v) = pure v
  readValue v = Left ("Not a Bool:" <> (show v))

instance ReadValue Text where
  readValue (AST.ValueString (AST.StringValue v)) = pure v
  readValue v = Left ("Not a String:" <> (show v))

instance forall v. ReadValue v => ReadValue [v] where
  readValue (AST.ValueList (AST.ListValue values)) = traverse (readValue @v) values
  readValue v = Left ("Not a List:" <> (show v))

instance forall v. ReadValue v => ReadValue (Maybe v) where
  valueMissing _ = pure Nothing
  readValue v = map Just (readValue @v v)

-- TODO: variables should error, they should have been resolved already.
--
-- TODO: Objects. Maybe implement some Generic object reader? I.e. if I do
-- data Greet = Greet { name :: Text, score :: Int } deriving Generic
-- then "instance ReadValue Greet" would fall back on a default reader that
-- expects Objects?
-- Maybe we can use advanced fallbacks like these:
-- https://wiki.haskell.org/GHC/AdvancedOverlap

class (MonadThrow m, MonadIO m) => BuildFieldResolver m a where
  type FieldHandler m a :: Type
  buildFieldResolver :: FieldHandler m a -> AST.Selection -> (Text, m GValue.Value)

instance forall ks t m. (KnownSymbol ks, HasGraph m t, MonadThrow m, MonadIO m) => BuildFieldResolver m (Field ks t) where
  type FieldHandler m (Field ks t) = HandlerType m t
  buildFieldResolver handler (AST.SelectionField (AST.Field _ _ _ _ selectionSet)) =
    let childResolver = buildResolver @m @t handler selectionSet
        name = toS (symbolVal (Proxy :: Proxy ks))
    in (name, childResolver)
  buildFieldResolver _ f = ("x", queryError ("buildFieldResolver got non AST.Field" <> show f <> ", query probably not normalized"))


instance forall ks t f m. (MonadThrow m, KnownSymbol ks, BuildFieldResolver m f, ReadValue t) => BuildFieldResolver m (Argument ks t :> f) where
  type FieldHandler m (Argument ks t :> f) = t -> FieldHandler m f
  buildFieldResolver handler selection@(AST.SelectionField (AST.Field _ _ arguments _ _)) =
    let argName = toS (symbolVal (Proxy :: Proxy ks))
        v = lookupValue argName arguments
    in case v of
       Nothing -> case valueMissing @t argName of
                     Left err' -> ("", queryError err')
                     Right v' -> buildFieldResolver @m @f (handler v') selection
       Just v' -> case readValue @t v' of
                    Left err' -> ("", queryError err')
                    Right v'' -> buildFieldResolver @m @f (handler v'') selection
  buildFieldResolver _ f = ("y", queryError ("buildFieldResolver got non AST.Field" <> show f <> ", query probably not normalized"))


class RunFields m a where
  type RunFieldsType m a :: Type
  -- Runfield is run on a single QueryTerm so it can only ever return
  -- one (Text, Value)
  runFields :: RunFieldsType m a -> AST.Selection -> m (Text, GValue.Value)


instance forall f fs m.
         ( BuildFieldResolver m f
         , RunFields m fs
         ) => RunFields m (f:fs) where
  type RunFieldsType m (f:fs) = (FieldHandler m f) :<> (RunFieldsType m fs)
  -- Deconstruct object type signature and handler value at the same
  -- time and run type-directed code for each field.
  runFields (lh :<> rh) selection@(AST.SelectionField (AST.Field alias name _ _ _)) =
    let (k, valueIO) = buildFieldResolver @m @f lh selection
    in case name == k of
      True -> do
        -- execute action to retrieve field value
        value <- valueIO
        -- NB "alias" is encoded in-band. It cannot be set to empty in
        -- a query so the empty value means "no alias" and we use the
        -- name instead.
        pure (if alias == "" then name else alias, value)
      False -> runFields @m @fs rh selection

  runFields _ f = queryError ("Unexpected Selection value. Is the query normalized?: " <> show f)

instance forall m. MonadThrow m => RunFields m '[] where
  type RunFieldsType m '[] = ()
  runFields _ selection = queryError ("Query for undefined selection:" <> (show selection))


instance forall typeName interfaces fields m.
         ( RunFields m fields
         , MonadThrow m
         , MonadIO m
         ) => HasGraph m (Object typeName interfaces fields) where
  type HandlerType m (Object typeName interfaces fields) = m (RunFieldsType m fields)

  buildResolver handlerIO selectionSet = do
    -- First we run the actual handler function itself in IO.
    handler <- handlerIO
    -- considering that this is an object we'll need to collect (name,
    -- Value) pairs from runFields and build a map with them.

    -- might need https://hackage.haskell.org/package/linkedhashmap to
    -- keep insertion order. (TODO)
    r <- forM selectionSet $ \selection -> runFields @m @fields handler selection
    pure $ GValue.toValue $ M.fromList r
