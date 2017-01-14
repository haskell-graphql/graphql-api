module OrderedMapTests (tests) where

import Protolude

import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified Data.Map as Map
import GraphQL.Internal.OrderedMap (OrderedMap)
import qualified GraphQL.Internal.OrderedMap as OrderedMap


orderedMaps :: Gen (OrderedMap Int Int)
orderedMaps = arbitrary

tests :: IO TestTree
tests = testSpec "OrderedMap" $ do
  describe "Integrity" $ do
    prop "fromList . toList == id" $ do
      forAll orderedMaps (\x -> OrderedMap.orderedMap (OrderedMap.toList x) == Just x)
    prop "keys == Map.keys . toMap" $ do
      forAll orderedMaps (\x -> sort (OrderedMap.keys x) == sort (Map.keys (OrderedMap.toMap x)))
    prop "keys == map fst . Map.toList" $ do
      forAll orderedMaps (\x -> OrderedMap.keys x == map fst (OrderedMap.toList x))
    prop "has unique keys" $ do
      forAll orderedMaps (\x -> let ks = OrderedMap.keys x in ks == ordNub ks)
    prop "all keys can be looked up" $ do
      forAll orderedMaps (\x -> let keys = OrderedMap.keys x
                                    values = OrderedMap.values x
                                in mapMaybe (flip OrderedMap.lookup x) keys == values)
    it "empty is orderedMap []" $ do
      Just (OrderedMap.empty @Int @Int) `shouldBe` OrderedMap.orderedMap []
    prop "singleton x is orderedMap [x]" $ do
      \x y -> Just (OrderedMap.singleton @Int @Int x y) == OrderedMap.orderedMap [(x, y)]
    it "preserves insertion order" $ do
      let items1 = [("foo", 2), ("bar", 1)]
      let Just x = OrderedMap.orderedMap items1
      OrderedMap.toList @Text @Int x `shouldBe` items1
      let items2 = [("bar", 1), ("foo", 2)]
      let Just y = OrderedMap.orderedMap items2
      OrderedMap.toList @Text @Int y `shouldBe` items2
