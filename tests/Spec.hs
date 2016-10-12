module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, TestTree, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "hello-prometheus-haskell" []
