-- | Data structure for mapping keys to values while preserving order of appearance.
--
-- There are many cases in GraphQL where we want to have a map from names to
-- values, where values can easily be lookup up by name and name is unique.
-- This would normally be modelled as a 'Map'. However, in many of these
-- cases, the order in which the entries appear matters.
--
-- That is,
--
-- @
-- {
--   'foo': 1,
--   'bar': 2
-- }
-- @
--
-- Is different to,
--
-- @
-- {
--   'bar': 2,
--   'foo': 1,
-- }
--
-- Even though they have exactly the same keys, and the keys have exactly the
-- same values.
--
-- Goal for this module is to provide data structures that are "complete
-- enough" for implementing the rest of GraphQL.
module GraphQL.Internal.OrderedMap
  ( OrderedMap
  -- * Construction
  , empty
  , singleton
  , orderedMap
  -- * Querying
  , lookup
  -- * Combine
  -- ** Union
  , unions
  -- * Conversion
  , toList
  , keys
  , values
  -- * Properties
  , prop_orderedMap
  ) where

import Protolude hiding (empty, toList)

import Test.QuickCheck (Arbitrary(..), listOf)

newtype OrderedMap key value = OrderedMap [(key, value)] deriving (Eq, Ord, Show)

-- | An ordered map is guaranteed to have a set of keys which are unique.
prop_orderedMap :: Ord key => OrderedMap key value -> Bool
prop_orderedMap om = ks == ordNub ks
  where
    ks = keys om

-- | Convert an ordered map to a list of keys and values. The list is
-- guaranteed to be the same order as the order of insertion into the map.
toList :: OrderedMap key value -> [(key, value)]
toList (OrderedMap entries) = entries

instance Foldable (OrderedMap key) where
  foldr f z om = foldr (f . snd) z (toList om)

instance Traversable (OrderedMap key) where
  traverse f om = OrderedMap <$> traverse (\(k, v) -> (,) k <$> f v) (toList om)

instance Functor (OrderedMap key) where
  fmap f = OrderedMap . map (second f) . toList

instance (Arbitrary key, Arbitrary value, Ord key) => Arbitrary (OrderedMap key value) where
  arbitrary = OrderedMap <$> entries
    where
      entries = zip <$> ks <*> vs
      ks = ordNub <$> listOf arbitrary
      vs = listOf arbitrary

-- | The empty OrderedMap.
empty :: OrderedMap key value
empty = OrderedMap []

-- | Find a value in an ordered map.
--
-- /O(n)/
lookup :: (Eq key, Show key, Show value) => key -> OrderedMap key value -> Maybe value
lookup key om =
  -- TODO: Add a Map structure to OrderedMap and use that for lookup, removing
  -- the 'panic', the 'Show' constraints, and making this 'O(log n)'.
  case [ v | (k, v) <- toList om, k == key ] of
    [] -> Nothing
    [v] -> Just v
    _ -> panic ("Found multiple values of " <> show key <> " in " <> show om)

-- | Create an ordered map containing a single entry.
singleton :: key -> value -> OrderedMap key value
singleton key value = OrderedMap [(key, value)]

-- | Get the list of keys from an ordered map, in order of appearance.
--
-- This list is guaranteed to have no duplicates.
keys :: OrderedMap key value -> [key]
keys = map fst . toList

-- | Get the values from an ordered map, in order of appearance.
values :: OrderedMap key value -> [value]
values = map snd . toList

-- | The union of a list of ordered maps.
--
-- If any map shares a key with any other map, return 'Nothing'.
--
-- Otherwise, return a new map containing all of the keys from all of the
-- maps. The keys from the first map will appear first, followed by the
-- second, and so forth.
unions :: Ord key => [OrderedMap key value] -> Maybe (OrderedMap key value)
unions orderedMaps = orderedMap (orderedMaps >>= toList)

-- | Construct an ordered map from a list.
--
-- If the list contains duplicate keys, then return 'Nothing'.
--
-- /O(n log n)/.
--
-- >>> orderedMap [("foo", 1), ("foo", 2)]
-- Nothing
--
-- Otherwise, return an 'OrderedMap', preserving the order.
--
-- >>> orderedMap [("foo",2),("bar",1)]
-- Just (OrderedMap [("foo",2),("bar",1)])
--
-- >>> orderedMap [("bar",1),("foo",2)]
-- Just (OrderedMap [("bar",1),("foo",2)])
orderedMap :: Ord key => [(key, value)] -> Maybe (OrderedMap key value)
orderedMap entries
  | ks == ordNub ks = Just (OrderedMap entries)
  | otherwise = Nothing
  where
    ks = map fst entries
