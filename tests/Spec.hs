module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified ASTTests
import qualified TypeApiTests
import qualified TypeTests
import qualified ValidationTests
import qualified ValueTests

-- import examples to ensure they compile
import Examples.UnionExample ()
import Examples.FileSystem ()

main :: IO ()
main = do
  t <- sequence tests
  defaultMain . testGroup "GraphQL API" $ t
  where
    tests =
      [ ASTTests.tests
      , TypeApiTests.tests
      , TypeTests.tests
      , ValidationTests.tests
      , ValueTests.tests
      ]
