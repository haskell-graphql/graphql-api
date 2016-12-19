module Main (main) where

import Protolude

import Criterion.Main (bgroup, defaultMain)
import qualified Validation


main :: IO ()
main = do
  defaultMain [ bgroup "GraphQL API" Validation.benchmarks
              ]
