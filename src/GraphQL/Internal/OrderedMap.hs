{-# LANGUAGE RankNTypes #-}
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
  , toMap
  , keys
  , values
  -- * Properties
  , genOrderedMap
  ) where

import Protolude hiding (empty, toList)

import qualified Data.Map as Map
import Test.QuickCheck (Arbitrary(..), Gen, listOf)

data OrderedMap key value
  = OrderedMap
    { -- | Get the list of keys from an ordered map, in order of appearance.
      --
      -- This list is guaranteed to have no duplicates.
      keys :: [key]
      -- | Convert an ordered map to a regular map, losing insertion order.
    , toMap :: Map key value
    }
  deriving (Eq, Ord, Show)

-- | Convert an ordered map to a list of keys and values. The list is
-- guaranteed to be the same order as the order of insertion into the map.
--
-- /O(n log n)/
toList :: forall key value. Ord key => OrderedMap key value -> [(key, value)]
toList (OrderedMap keys entries) = catMaybes (foreach keys $ \k -> (,) k <$> Map.lookup k entries)

instance Foldable (OrderedMap key) where
  foldr f z (OrderedMap _ entries) = foldr f z entries

instance Traversable (OrderedMap key) where
  traverse f (OrderedMap keys entries) = OrderedMap keys <$> traverse f entries

instance Functor (OrderedMap key) where
  fmap f (OrderedMap keys entries) = OrderedMap keys (map f entries)

instance (Arbitrary key, Arbitrary value, Ord key) => Arbitrary (OrderedMap key value) where
  arbitrary = genOrderedMap arbitrary arbitrary

-- | Generate an ordered map with the given key & value generators.
genOrderedMap :: forall key value. Ord key => Gen key -> Gen value -> Gen (OrderedMap key value)
genOrderedMap genKey genValue = do
  entries <- Map.fromList <$> (zip <$> listOf genKey <*> listOf genValue)
  pure (OrderedMap (Map.keys entries) entries)

-- | The empty OrderedMap. /O(1)/
empty :: forall key value. OrderedMap key value
empty = OrderedMap [] Map.empty

-- | Create an ordered map containing a single entry. /O(1)/
singleton :: forall key value. key -> value -> OrderedMap key value
singleton key value = OrderedMap [key] (Map.singleton key value)

-- | Find a value in an ordered map.
--
-- /O(log n)/
lookup :: forall key value. Ord key => key -> OrderedMap key value -> Maybe value
lookup key (OrderedMap _ entries) = Map.lookup key entries

-- | Get the values from an ordered map, in order of appearance. /O(n log n)/
values :: forall key value. Ord key => OrderedMap key value -> [value]
values = map snd . toList

-- | The union of a list of ordered maps.
--
-- If any map shares a key with any other map, return 'Nothing'.
--
-- Otherwise, return a new map containing all of the keys from all of the
-- maps. The keys from the first map will appear first, followed by the
-- second, and so forth.
--
-- /O(m * n log (m * n))/ where /m/ is the number of maps, and /n/ is the size of
-- the largest map.
unions :: forall key value. Ord key => [OrderedMap key value] -> Maybe (OrderedMap key value)
unions orderedMaps = orderedMap (orderedMaps >>= toList)

-- | Construct an ordered map from a list.
--
-- /O(n log n)/.
--
-- If the list contains duplicate keys, then return 'Nothing'. Otherwise,
-- return an 'OrderedMap', preserving the order.
orderedMap :: forall key value. Ord key => [(key, value)] -> Maybe (OrderedMap key value)
orderedMap entries
  | ks == ordNub ks = Just (OrderedMap ks (Map.fromList entries))
  | otherwise = Nothing
  where
    ks = map fst entries
