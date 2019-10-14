{-# LANGUAGE DataKinds #-}
module Main where

import Protolude

import Network.Wai.Test
import GraphQL.API
import GraphQL.Wai
import GraphQL.Resolver

type Cat = Object "Cat" '[] '[Field "name" Text]

catHandler :: Handler IO Cat
catHandler = pure (returns "Felix")

test1 :: Session ()
test1 = do
  r <- request $ setPath defaultRequest "/?query={ name }"
  assertStatus 200 r
  assertBody "{\"data\":{\"name\":\"Felix\"}}" r

main :: IO ()
main = do
  void $ runSession test1 (toApplication @Cat catHandler)
