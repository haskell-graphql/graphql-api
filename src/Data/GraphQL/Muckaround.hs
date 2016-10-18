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
