-- | Launch graphql-haskell server.
module Main
  ( main
  ) where

import Protolude

import GraphQL.Server (startApp)

main :: IO ()
main = startApp
