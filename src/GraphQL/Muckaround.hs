{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Example from Servant paper:
--
-- http://alpmestan.com/servant/servant-wgp.pdf
module GraphQL.Muckaround (One, (:+), Hole, valueOf) where

import Protolude

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.GraphQL.AST as AST
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownSymbol, symbolVal)

data One
data e1 :+ e2
data Hole

class HasValue a where
  type Value a r :: *
  valOf :: Proxy a -> (Int -> r) -> Value a r

instance HasValue One where
  type Value One r = r
  valOf _ ret = ret 1

instance (HasValue e1, HasValue e2) => HasValue (e1 :+ e2) where
  type Value (e1 :+ e2) r = Value e1 (Value e2 r)

  valOf _ ret = valOf (Proxy :: Proxy e1) (\v1 ->
                valOf (Proxy :: Proxy e2) (\v2 -> ret (v1 + v2)))

instance HasValue Hole where
  type Value Hole r = Int -> r
  valOf _ ret = ret


valueOf :: HasValue a => Proxy a -> Value a Int
valueOf p = valOf p identity


-- | A query that has all its fragments, variables, and directives evaluated,
-- so that all that is left is a query with literal values.
--
-- 'SelectionSet' is maybe the closest type, but isn't quite what we want, as
-- it still has places for directives and other symbolic values.
type CanonicalQuery = AST.SelectionSet

-- | GraphQL responses are JSON values: objects, to be precise. They have a
-- "data" key and an "errors" key.
type Response = Aeson.Value

-- | A GraphQL application takes a canonical query and returns a response.
-- XXX: Really unclear what type this should be. Does it need IO? Generic
-- across Monad? Something analogous to the continuation-passing style of
-- WAI.Application?
type Application = CanonicalQuery -> IO Response


-- | A field within an object.
--
-- e.g.
--  "foo" :> Foo
data (name :: k) :> a deriving (Typeable)

-- XXX: This structure is cargo-culted from Servant, even though jml doesn't fully
-- understand it yet.
type Handler = IO
type Graphable api = GraphT api Handler

class HasGraph api where
  type GraphT api (m :: * -> *) :: *
  resolve :: Proxy api -> Graphable api -> Application

-- | A field within an object.
--
-- e.g.
--  "foo" :> Foo
instance (KnownSymbol name, HasGraph api) => HasGraph (name :> api) where
  type GraphT (name :> api) m = GraphT api m

  resolve Proxy subApi query =
    case lookup query fieldName of
      Nothing -> empty
      Just (alias, subQuery) -> buildField alias (resolve (Proxy :: Proxy api) subApi subQuery)
    where
      fieldName = toS (symbolVal (Proxy :: Proxy name))
      lookup q f = listToMaybe [ (a, s) | AST.SelectionField (AST.Field a n [] _ s) <- q
                                        , n == f
                                        ]
      buildField alias' value = do
        value' <- value
        -- XXX: An object? Really?
        pure (Aeson.object [alias' .= value'])
