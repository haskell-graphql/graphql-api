module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified TypeApiTests
import qualified TypeTests
import qualified ValidationTests
import qualified ValueTests

-- import examples to ensure they compile
import Examples.UnionExample ()

main :: IO ()
main = do
  t <- sequence tests
  defaultMain . testGroup "GraphQL API" $ t
  where
    tests =
      [ TypeApiTests.tests
      , TypeTests.tests
      , ValidationTests.tests
      , ValueTests.tests
      ]
