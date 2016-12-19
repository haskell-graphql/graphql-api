{-# LANGUAGE TypeApplications #-}
module Validation (benchmarks) where

import Protolude

import Criterion (Benchmark, bench, nf)
import GraphQL.Internal.Validation (findDuplicates)


benchmarks :: [Benchmark]
benchmarks =
  [ bench "findDuplicates" (nf findDuplicates exampleData)
  ]
  where
    exampleData :: [Int]
    exampleData = [2, 8, 9, 8, 1, 7, 5, 0, 1, 3, 5, 4]
