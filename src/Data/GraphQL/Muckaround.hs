{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Example from Servant paper:
--
-- http://alpmestan.com/servant/servant-wgp.pdf
module Data.GraphQL.Muckaround (One, (:+), Hole, valueOf) where

import Protolude

import Data.Proxy (Proxy)

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


-- XXX: In Servant, RoutingApplication is a thin wrapper over WAI.Application.
-- Maybe we want to return a WAI Application, but it seems to me (jml) that we
-- could just as well have something non-webby.
--
-- XXX: Really unclear what type this should be. Does it need IO? Generic
-- across Monad? Something analogous to the continuation-passing style of
-- WAI.Application?

{-
data CanonicalQuery
data GraphQLResponse
type GraphQLApplication = CanonicalQuery -> IO GraphQLResponse

-- | A field within an object.
--
-- e.g.
--  "foo" :> Foo
data (name :: k) :> a deriving (Typeable)

class HasGraph api where
  type Graph api r :: *
  resolve :: Proxy api -> Graph api r -> GraphQLApplication

instance (KnownSymbol name, HasGraph api) => HasGraph (name :> api) where
  type Graph (name :> api) r = Graph api r
  resolve Proxy subApi query =
    case lookup query fieldName of
      Nothing -> empty
      Just subQuery -> buildField fieldName (resolve (Proxy :: api) subApi subQuery)
-}
