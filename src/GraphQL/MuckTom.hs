{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GraphQL.MuckTom where

-- TODO (probably incomplete, the spec is large)
-- * input objects - I'm not super clear from the spec on how
--   they differ from normal objects.
-- * "extend type X" is used in examples in the spec but it's not
--   explained anywhere?
-- * Directives (https://facebook.github.io/graphql/#sec-Type-System.Directives)
-- * Enforce non-empty lists (might only be doable via value-level validation)

import qualified Prelude

import GraphQL.Schema hiding (Type)
import qualified GraphQL.Schema (Type)
import Protolude hiding (Enum)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified GHC.TypeLits (TypeError, ErrorMessage(..))

import qualified GraphQL.Value as GValue
import qualified Data.Map as M

import Data.GraphQL.Parser (document)
import Data.Attoparsec.Text (parseOnly, endOfInput)

import qualified Control.Monad.Trans.Except as E

import GraphQL.Definitions

-- import Data.GraphQL.AST (SelectionSet, Selection(..), Field(..), Alias, Name, Argument(..), Value(..))
import qualified Data.GraphQL.AST as AST


-- TODO instead of SelectionSet we want something like
-- NormalizedSelectionSet which has query fragments etc. resolved.
class MonadIO m => HasGraph m a where
  type HandlerType m a
  buildResolver :: HandlerType m a -> AST.SelectionSet -> m GValue.Value


-- Parse a value of the right type from an argument
class ReadValue a where
  readValue :: AST.Name -> [AST.Argument] -> a


-- TODO not super hot on individual values having to be instances of
-- HasGraph but not sure how else we can nest either types or
-- (Object _ _ fields). Maybe instead of field we need a "SubObject"?
instance forall m. MonadIO m => HasGraph m Int32 where
  type HandlerType m Int32 = m Int32
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  fmap GValue.toValue handler


instance forall m. MonadIO m => HasGraph m Double where
  type HandlerType m Double = m Double
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  fmap GValue.toValue handler


-- TODO: lookup is O(N^2) in number of arguments (we linearly search
-- each argument in the list) but considering the graphql use case
-- where N usually < 10 this is probably OK.
-- TODO (Maybe Int) types that can convert Nothing
instance ReadValue Int32 where
  readValue name args =
    case find (\(AST.Argument name' _) -> name' == name) args of
      Nothing -> error ("Mandatory Int argument of name " <> name <> "not provided by query")
      Just (AST.Argument _ value) -> case value of
        AST.ValueInt v -> v
        _ -> error ("Not an Int:" <> (show value))

instance ReadValue Double where
  readValue name args =
    case find (\(AST.Argument name' _) -> name' == name) args of
      Nothing -> error ("Mandatory Float argument of name " <> name <> "not provided by query")
      Just (AST.Argument _ value) -> case value of
        AST.ValueFloat v -> v
        _ -> error ("Not a Float:" <> (show value))

-- TODO plug in remaining values from: https://hackage.haskell.org/package/graphql-0.3/docs/Data-GraphQL-AST.html#t:Value

class MonadIO m => BuildFieldResolver m a where
  type FieldHandler m a :: Type
  buildFieldResolver :: FieldHandler m a -> AST.Selection -> (Text, m GValue.Value)


instance forall ks t m. (KnownSymbol ks, HasGraph m t, MonadIO m) => BuildFieldResolver m (Field ks t) where
  type FieldHandler m (Field ks t) = HandlerType m t
  buildFieldResolver handler selection@(AST.SelectionField (AST.Field alias name arguments directives selectionSet)) =
    let childResolver = buildResolver @m @t handler selectionSet
        name = toS (symbolVal (Proxy :: Proxy ks))
    in (name, childResolver)


instance forall ks t f m. (KnownSymbol ks, BuildFieldResolver m f, ReadValue t) => BuildFieldResolver m (Argument ks t :> f) where
  type FieldHandler m (Argument ks t :> f) = t -> FieldHandler m f
  buildFieldResolver handler selection@(AST.SelectionField (AST.Field alias name arguments directives selectionSet)) =
    let argName = toS (symbolVal (Proxy :: Proxy ks))
        partiallyAppliedHandler = handler (readValue @t argName arguments)
    in buildFieldResolver @m @f partiallyAppliedHandler selection


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
  runFields handler@(lh :<> rh) selection@(AST.SelectionField (AST.Field alias name arguments directives selectionSet)) =
    let (k, valueIO) = buildFieldResolver @m @f lh selection -- TODO query args
    in case name == k of
      True -> do
        value <- valueIO
        pure (if alias == "" then name else alias, value)
      False -> runFields @m @fs rh selection

  runFields _ f = error ("Unexpected Selection value. Is the query normalized?: " <> show f)


instance RunFields m '[] where
  type RunFieldsType m '[] = ()
  -- TODO maybe better to return Either?
  runFields _ selection = error ("Query for undefined selection:" <> (show selection))


-- TODO: arguments. The interpreter
instance forall typeName interfaces fields m.
         ( RunFields m fields
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


-- custom error hander example:
-- E.runExceptT $ buildResolver @TMonad @T tHandler tQuery

type TMonad = E.ExceptT Text IO
--- test code below
type T = Object "T" '[] '[Field "z" Int32, Argument "t" Int32 :> Field "t" Int32]


tHandler :: HandlerType TMonad T
tHandler = do
  conn <- liftIO $ print @IO @Text "HI"
  pure $ (pure 10) :<> (\tArg -> pure tArg) :<> ()


tQuery =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) = parseOnly (document <* endOfInput) "{ t(t: 12) }"
  in selectionSet

-- hlist :: a -> (a :<> ()) TODO
-- hlist a = a :<> ()

type Calculator = Object "Calculator" '[]
  '[ Argument "a" Int32 :> Argument "b" Int32 :> Field "add" Int32
   , Argument "a" Double :> Field "log" Double
   ]

type API = Object "API" '[] '[Field "calc" Calculator]

type FakeUser = ()

calculatorHandler :: FakeUser -> HandlerType IO Calculator
calculatorHandler _fakeUser =
  pure (add' :<> log' :<> ())
  where
    add' a b = pure (a + b)
    log' a = pure (log a)

api :: HandlerType IO API
api = do
  fakeUser <- print @IO @Text "fake lookup user"
  pure (calculatorHandler fakeUser :<> ())

-- Use like: `buildResolver @IO @Calculator (calculatorHandler ()) calculatorQuery`
calculatorQuery =
  let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ selectionSet))]) =
        parseOnly (document <* endOfInput) "{ add(a: 1, b: 2) }"
  in selectionSet
