-- | This module provides an efficient implementation of maps from 'Int' keys to values.
-- It is a thin wrapper around 'Data.HashCons.WordMap', converting 'Int' keys to 'Word'.

module Data.HashCons.IntMap
  ( IntMap
  , empty
  , singleton
  , singletonNonEmpty
  , insert
  , insertNonEmpty
  , lookup
  , lookupNonEmpty
  , map
  , mapWithKey
  , traverseWithKey
  , union
  , unionNonEmpty
  , intersection
  , intersectionNonEmpty
  , difference
  , differenceNonEmpty
  , member
  ) where

import Prelude hiding (lookup, map)
import Data.HashCons.WordMap (WordMap, NonEmptyWordMap)
import qualified Data.HashCons.WordMap as WordMap
import Data.Word
import Data.Hashable

newtype IntMap a = IntMap { unWordMap :: WordMap a }

newtype NonEmptyIntMap a = NonEmptyIntMap { unNonEmptyWordMap :: NonEmptyWordMap a }

-- | /O(1)/. Create an empty map.
empty :: IntMap a
empty = IntMap WordMap.empty

-- | /O(1)/. Create a map with a single key-value pair.
singleton :: Hashable a => Int -> a -> IntMap a
singleton k v = IntMap $ WordMap.singleton (fromIntegral k) v

-- | /O(1)/. Create a map with a single key-value pair.
singletonNonEmpty :: Hashable a => Int -> a -> NonEmptyIntMap a
singletonNonEmpty k v = NonEmptyIntMap $ WordMap.singletonNonEmpty (fromIntegral k) v

-- | /O(log n)/. Insert a key-value pair into the map.
insert :: Hashable a => Int -> a -> IntMap a -> IntMap a
insert k v (IntMap m) = IntMap $ WordMap.insert (fromIntegral k) v m

-- | /O(log n)/. Insert a key-value pair into the map.
insertNonEmpty :: Hashable a => Int -> a -> NonEmptyIntMap a -> NonEmptyIntMap a
insertNonEmpty k v (NonEmptyIntMap m) = NonEmptyIntMap $ WordMap.insertNonEmpty (fromIntegral k) v m

-- | /O(log n)/. Lookup the value at a key in the map.
lookup :: Int -> IntMap a -> Maybe a
lookup k (IntMap m) = WordMap.lookup (fromIntegral k) m

-- | /O(log n)/. Lookup the value at a key in the map.
lookupNonEmpty :: Int -> NonEmptyIntMap a -> Maybe a
lookupNonEmpty k (NonEmptyIntMap m) = WordMap.lookupNonEmpty (fromIntegral k) m

-- | /O(n)/. Map a function over all values in the map.
map :: Hashable b => (a -> b) -> IntMap a -> IntMap b
map f (IntMap m) = IntMap $ WordMap.map f m

-- | /O(n)/. Map a function over all key-value pairs in the map.
mapWithKey :: Hashable b => (Int -> a -> b) -> IntMap a -> IntMap b
mapWithKey f (IntMap m) = IntMap $ WordMap.mapWithKey (\k v -> f (fromIntegral k) v) m

-- | /O(n)/. Traverse the map with a function that accesses keys.
traverseWithKey :: (Applicative t, Hashable b) => (Int -> a -> t b) -> IntMap a -> t (IntMap b)
traverseWithKey f (IntMap m) = IntMap <$> WordMap.traverseWithKey (\k v -> f (fromIntegral k) v) m

-- | /O(n)/. The union of two maps. If a key occurs in both maps, the value from the left map is used.
union :: Hashable a => IntMap a -> IntMap a -> IntMap a
union (IntMap m1) (IntMap m2) = IntMap $ WordMap.union m1 m2

-- | /O(n)/. The union of two maps. If a key occurs in both maps, the value from the left map is used.
unionNonEmpty :: Hashable a => NonEmptyIntMap a -> IntMap a -> NonEmptyIntMap a
unionNonEmpty (NonEmptyIntMap m1) (IntMap m2) = NonEmptyIntMap $ WordMap.unionNonEmpty m1 m2

-- | /O(n)/. Intersection of two maps. Only keys present in both maps are included.
intersection :: Hashable a => IntMap a -> IntMap a -> IntMap a
intersection (IntMap m1) (IntMap m2) = IntMap $ WordMap.intersection m1 m2

-- | /O(n)/. Intersection of two maps. Only keys present in both maps are included.
intersectionNonEmpty :: Hashable a => NonEmptyIntMap a -> IntMap a -> IntMap a
intersectionNonEmpty (NonEmptyIntMap m1) (IntMap m2) = IntMap $ WordMap.intersectionNonEmpty m1 m2

-- | /O(n)/. Difference of two maps. Keys in the first map but not in the second are included.
difference :: Hashable a => IntMap a -> IntMap a -> IntMap a
difference (IntMap m1) (IntMap m2) = IntMap $ WordMap.difference m1 m2

-- | /O(n)/. Intersection of two maps. Only keys present in both maps are included.
differenceNonEmpty :: Hashable a => NonEmptyIntMap a -> IntMap a -> IntMap a
differenceNonEmpty (NonEmptyIntMap m1) (IntMap m2) = IntMap $ WordMap.differenceNonEmpty m1 m2

-- | Check if a key is present in the map.
member :: Int -> IntMap a -> Bool
member k (IntMap m) = WordMap.member (fromIntegral k) m

-- | Check if a key is present in the map.
memberNonEmpty :: Int -> NonEmptyIntMap a -> Bool
memberNonEmpty k (NonEmptyIntMap m) = WordMap.memberNonEmpty (fromIntegral k) m
