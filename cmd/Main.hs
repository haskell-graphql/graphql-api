{-# LANGUAGE TypeOperators #-}

-- | Run graphql-api experiments.
module Main
  ( main
  ) where

import Protolude

import Data.GraphQL.Muckaround (One, (:+), Hole, valueOf)

type Two = One :+ One
type Holes = Hole :+ One :+ Hole


main :: IO ()
main = do
  let t1 = valueOf (Proxy :: Proxy Two)
  let t2 = valueOf (Proxy :: Proxy Holes)
  putText $ "t1 = " <> show t1
  putText $ "t2 3 4 = " <> show (t2 3 4)
