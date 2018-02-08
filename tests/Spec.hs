module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified ASTTests
import qualified EndToEndTests
import qualified OrderedMapTests
import qualified ResolverTests
import qualified SchemaTests
import qualified ValidationTests
import qualified ValueTests
import qualified EnumTests ()

main :: IO ()
main = do
  t <- sequence tests
  defaultMain . testGroup "GraphQL API" $ t
  where
    tests =
      [ ASTTests.tests
      , EndToEndTests.tests
      , OrderedMapTests.tests
      , ResolverTests.tests
      , SchemaTests.tests
      , ValidationTests.tests
      , ValueTests.tests
      ]
