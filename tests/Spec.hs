module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified ValidationTests
import qualified TypeTests
import qualified TypeApiTests

-- import examples to ensure they compile
import Examples.UnionExample ()

main :: IO ()
main = do
  t <- sequence [ ValidationTests.tests
                , TypeTests.tests
                , TypeApiTests.tests
                ]
  defaultMain (testGroup "spec" t)
