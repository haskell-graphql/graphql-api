{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeFamilyDependencies #-}
{-# LANGUAGE GADTs, AllowAmbiguousTypes, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeApplications, TypeInType #-}
{-# LANGUAGE OverloadedLabels, MagicHash #-}

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

import qualified GraphQL.Value as Value
import qualified Data.Map as M

import GraphQL.Definitions


-- simple text placeholder query for now
type QueryTerm = Text
type QueryArgs = [Text]
type Query = [QueryTerm] -- also know as a `SelectionSet`

class HasGraph a where
  type HandlerType a
  buildResolver :: HandlerType a -> Query -> IO Value.Value

-- TODO not super hot on individual values having to be instances of
-- HasGraph but not sure how else we can nest either types or
-- (Object _ _ fields). Maybe instead of field we need a "SubObject"?
instance HasGraph Int32 where
  type HandlerType Int32 = IO Int32
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  fmap Value.toValue handler


instance HasGraph Double where
  type HandlerType Double = IO Double
  -- TODO check that selectionset is empty (we expect a terminal node)
  buildResolver handler _ =  fmap Value.toValue handler


-- Parse a value of the right type from an argument
class ReadValue a where
  readValue :: QueryArgs -> a

instance ReadValue Int where
  readValue _ = 14

instance ReadValue Int32 where
  readValue _ = 32

instance ReadValue Double where
  readValue _ = 14.0


class BuildFieldResolver a where
  type FieldHandler a :: Type
  buildFieldResolver :: FieldHandler a -> QueryArgs -> (Text, IO Value.Value)

instance forall ks t. (KnownSymbol ks, HasGraph t) => BuildFieldResolver (Field ks t) where
  type FieldHandler (Field ks t) = HandlerType t
  buildFieldResolver handler queryArgs =
    let childResolver = buildResolver @t handler [] -- need sub query access so can't just pass queryargs
        name = toS (symbolVal (Proxy :: Proxy ks))
    in (name, childResolver)

instance forall ks t f. (KnownSymbol ks, BuildFieldResolver f, ReadValue t) => BuildFieldResolver (Argument ks t :> f) where
  type FieldHandler (Argument ks t :> f) = t -> FieldHandler f
  buildFieldResolver handler queryArgs =
    let partiallyAppliedHandler = handler (readValue @t []) -- need query args access here
    in buildFieldResolver @f partiallyAppliedHandler queryArgs


class RunFields a where
  type RunFieldsType a :: Type
  -- Runfield is run on a single QueryTerm so it can only ever return
  -- one (Text, Value)
  runFields :: RunFieldsType a -> QueryTerm -> IO (Text, Value.Value)


instance forall f fs.
         ( BuildFieldResolver f
         , RunFields fs
         ) => RunFields (f:fs) where
  type RunFieldsType (f:fs) = (FieldHandler f) :<> (RunFieldsType fs)
  -- Deconstruct object type signature and handler value at the same
  -- time and run type-directed code for each field.
  runFields handler@(lh :<> rh) term =
    let (k, valueIO) = buildFieldResolver @f lh [] -- TODO query args
    in case term == k of
      True -> do
        value <- valueIO
        pure (k, value)
      False -> runFields @fs rh term


instance RunFields '[] where
  type RunFieldsType '[] = ()
  -- TODO maybe better to return Either?
  runFields _ term = error ("Query for undefined term:" <> (show term))


-- TODO: arguments. The interpreter
instance forall typeName interfaces fields.
         ( RunFields fields
         ) => HasGraph (Object typeName interfaces fields) where
  type HandlerType (Object typeName interfaces fields) = IO (RunFieldsType fields)

  buildResolver handlerIO query = do
    -- First we run the actual handler function itself in IO.
    handler <- handlerIO
    -- considering that this is an object we'll need to collect (name,
    -- Value) pairs from runFields and build a map with them.

    -- might need https://hackage.haskell.org/package/linkedhashmap to
    -- keep insertion order. (TODO)
    r <- forM query $ \term -> runFields @fields handler term
    pure $ Value.toValue $ M.fromList r


--- test code below
type T = Object "T" '[] '[Field "z" Int32, Argument "t" Int :> Field "t" Int32]

tHandler :: HandlerType T
tHandler = do
  conn <- print @IO @Text "HI"
  pure $ (pure 10) :<> (\tArg -> pure 10) :<> ()

-- hlist :: a -> (a :<> ()) TODO
-- hlist a = a :<> ()

type Calculator = Object "Calculator" '[]
  '[ Argument "a" Int32 :> Argument "b" Int32 :> Field "add" Int32
   , Argument "a" Double :> Field "log" Double
   ]

type API = Object "API" '[] '[Field "calc" Calculator]

type FakeUser = ()

calculatorHandler :: FakeUser -> HandlerType Calculator
calculatorHandler _fakeUser =
  pure (add' :<> log' :<> ())
  where
    add' a b = pure (a + b)
    log' a = pure (log a)

api :: HandlerType API
api = do
  fakeUser <- print @IO @Text "fake lookup user"
  pure (calculatorHandler fakeUser :<> ())
