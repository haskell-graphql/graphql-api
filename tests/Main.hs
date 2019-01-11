module Main where

import Protolude

import Test.Hspec
import qualified Spec (spec)

main :: IO ()
main = do
  hspec Spec.spec
