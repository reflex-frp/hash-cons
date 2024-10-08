{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module provides an efficient implementation of maps from 'Word' keys to values.
-- It uses a Patricia trie with hash-consing to enable fast structural sharing and
-- O(1) equality checks. The 'HashCons' type wraps the internal nodes to take advantage
-- of O(1) 'Eq' and 'Ord' operations provided by hash-consing.
--
-- Note: This is an internal module. Users should prefer the public interface provided
-- by 'Data.HashCons.WordMap'.

module Data.HashCons.WordMap.Internal where

import Prelude hiding (lookup, map)
import Data.Bits
import Data.Hashable
import Data.HashCons
import GHC.Generics

-- | The main 'WordMap' data type.
data WordMap a
    = Empty
    | NonEmptyMap !(HashCons (NonEmptyWordMap a))
    deriving (Show, Eq, Ord, Generic)

instance Hashable a => Hashable (WordMap a)

-- | Non-empty trie nodes.
data NonEmptyWordMap a
    = Leaf !Word a
    | Bin !Word !Word (WordMap a) (WordMap a)
    deriving (Show, Eq, Ord, Generic)

instance Hashable a => Hashable (NonEmptyWordMap a)

-- | /O(1)/. Create an empty map.
empty :: WordMap a
empty = Empty

-- | /O(1)/. Create a map with a single key-value pair.
singleton :: Hashable a => Word -> a -> WordMap a
singleton k v = NonEmptyMap (hashCons (Leaf k v))

-- | /O(1)/. Create a map with a single key-value pair.
singletonNonEmpty :: Hashable a => Word -> a -> NonEmptyWordMap a
singletonNonEmpty = Leaf

-- | /O(log n)/. Insert a key-value pair into the map.
-- If the key is already present, the value is replaced.
insert :: Hashable a => Word -> a -> WordMap a -> WordMap a
insert k v Empty = singleton k v
insert k v (NonEmptyMap hc) =
    let t = unHashCons hc
        t' = insertNonEmpty k v t
    in if t' == t  -- Exploit O(1) 'Eq' from 'HashCons'
       then NonEmptyMap hc  -- No change needed
       else NonEmptyMap (hashCons t')

insertNonEmpty :: Hashable a => Word -> a -> NonEmptyWordMap a -> NonEmptyWordMap a
insertNonEmpty k v t@(Leaf k' _)
  | k == k'   = Leaf k v  -- Replace the existing value
  | otherwise = join k (Leaf k v) k' t
insertNonEmpty k v t@(Bin p m l r)
  | match k p m = if zero k m
                  then
                    let l' = insert k v l
                    in if l' == l then t else Bin p m l' r
                  else
                    let r' = insert k v r
                    in if r' == r then t else Bin p m l r'
  | otherwise   = join k (Leaf k v) p t

-- | /O(log n)/. Lookup the value at a key in the map.
lookup :: Word -> WordMap a -> Maybe a
lookup _ Empty = Nothing
lookup k (NonEmptyMap hc) = lookupNonEmpty k (unHashCons hc)

lookupNonEmpty :: Word -> NonEmptyWordMap a -> Maybe a
lookupNonEmpty k (Leaf k' v)
  | k == k'   = Just v
  | otherwise = Nothing
lookupNonEmpty k (Bin p m l r)
  | match k p m = if zero k m then lookup k l else lookup k r
  | otherwise   = Nothing

-- | /O(n)/. Map a function over all values in the map.
map :: Hashable b => (a -> b) -> WordMap a -> WordMap b
map _ Empty = Empty
map f (NonEmptyMap hc) = NonEmptyMap (hashCons (mapNonEmpty f (unHashCons hc)))

mapNonEmpty :: Hashable b => (a -> b) -> NonEmptyWordMap a -> NonEmptyWordMap b
mapNonEmpty f (Leaf k v) = Leaf k (f v)
mapNonEmpty f (Bin p m l r) = Bin p m (map f l) (map f r)

-- | /O(n)/. Map a function over all key-value pairs in the map.
mapWithKey :: Hashable b => (Word -> a -> b) -> WordMap a -> WordMap b
mapWithKey _ Empty = Empty
mapWithKey f (NonEmptyMap hc) = NonEmptyMap (hashCons (mapWithKeyNonEmpty f (unHashCons hc)))

mapWithKeyNonEmpty :: Hashable b => (Word -> a -> b) -> NonEmptyWordMap a -> NonEmptyWordMap b
mapWithKeyNonEmpty f (Leaf k v) = Leaf k (f k v)
mapWithKeyNonEmpty f (Bin p m l r) = Bin p m (mapWithKey f l) (mapWithKey f r)

-- | /O(n)/. Traverse the map with a function that accesses keys.
traverseWithKey :: (Applicative t, Hashable b) => (Word -> a -> t b) -> WordMap a -> t (WordMap b)
traverseWithKey _ Empty = pure Empty
traverseWithKey f (NonEmptyMap hc) = NonEmptyMap . hashCons <$> traverseWithKeyNonEmpty f (unHashCons hc)

traverseWithKeyNonEmpty :: (Applicative t, Hashable b) => (Word -> a -> t b) -> NonEmptyWordMap a -> t (NonEmptyWordMap b)
traverseWithKeyNonEmpty f (Leaf k v) = Leaf k <$> f k v
traverseWithKeyNonEmpty f (Bin p m l r) = Bin p m <$> traverseWithKey f l <*> traverseWithKey f r

-- | /O(n)/. The union of two maps. If a key occurs in both maps, the value from the left map is used.
union :: Hashable a => WordMap a -> WordMap a -> WordMap a
union t1 Empty = t1
union Empty t2 = t2
union t1@(NonEmptyMap hc1) t2@(NonEmptyMap hc2)
  | hc1 == hc2 = t1  -- O(1) comparison via 'HashCons'
union (NonEmptyMap hc1) t2 =
    NonEmptyMap (hashCons (unionNonEmpty (unHashCons hc1) t2))

unionNonEmpty :: Hashable a => NonEmptyWordMap a -> WordMap a -> NonEmptyWordMap a
unionNonEmpty t1 Empty = t1
unionNonEmpty t1 (NonEmptyMap hc2) = unionNonEmptyNonEmpty t1 (unHashCons hc2)

unionNonEmptyNonEmpty :: Hashable a => NonEmptyWordMap a -> NonEmptyWordMap a -> NonEmptyWordMap a
unionNonEmptyNonEmpty t1 t2
  | t1 == t2  = t1  -- O(1) comparison via 'HashCons'
unionNonEmptyNonEmpty t1@(Leaf k1 v1) t2@(Leaf k2 _)
  | k1 == k2  = t1  -- Prefer left map's value
  | otherwise = join k1 t1 k2 t2
unionNonEmptyNonEmpty t1@(Leaf k1 v1) t2@(Bin p2 m2 l2 r2)
  | match k1 p2 m2 = if zero k1 m2
                     then Bin p2 m2 (union (singleton k1 v1) l2) r2
                     else Bin p2 m2 l2 (union (singleton k1 v1) r2)
  | otherwise      = join k1 t1 p2 t2
unionNonEmptyNonEmpty t1@(Bin p1 m1 l1 r1) t2@(Leaf k2 v2)
  | match k2 p1 m1 = if zero k2 m1
                     then Bin p1 m1 (union l1 (singleton k2 v2)) r1
                     else Bin p1 m1 l1 (union r1 (singleton k2 v2))
  | otherwise      = join p1 t1 k2 t2
unionNonEmptyNonEmpty t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | t1 == t2  = t1  -- O(1) comparison via 'HashCons'
  | shorter m1 m2 =
      if match p2 p1 m1
      then if zero p2 m1
           then Bin p1 m1 (union l1 (NonEmptyMap (hashCons t2))) r1
           else Bin p1 m1 l1 (union r1 (NonEmptyMap (hashCons t2)))
      else join p1 t1 p2 t2
  | shorter m2 m1 =
      if match p1 p2 m2
      then if zero p1 m2
           then Bin p2 m2 (union (NonEmptyMap (hashCons t1)) l2) r2
           else Bin p2 m2 l2 (union (NonEmptyMap (hashCons t1)) r2)
      else join p1 t1 p2 t2
  | p1 == p2      = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise     = join p1 t1 p2 t2

-- | /O(n)/. Intersection of two maps. Only keys present in both maps are included.
intersection :: Hashable a => WordMap a -> WordMap a -> WordMap a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection t1@(NonEmptyMap hc1) t2@(NonEmptyMap hc2)
  | hc1 == hc2 = t1  -- O(1) comparison via 'HashCons'
intersection (NonEmptyMap hc1) t2 =
    intersectionNonEmpty (unHashCons hc1) t2

intersectionNonEmpty :: Hashable a => NonEmptyWordMap a -> WordMap a -> WordMap a
intersectionNonEmpty _ Empty = Empty
intersectionNonEmpty t1 (NonEmptyMap hc2) =
    case t1 of
      Leaf k1 v1 -> if member k1 (NonEmptyMap hc2)
                    then singleton k1 v1
                    else Empty
      Bin p1 m1 l1 r1 -> case unHashCons hc2 of
        Leaf k2 a2
          | match k2 p1 m1 -> if zero k2 m1
                               then intersection l1 (singleton k2 a2)
                               else intersection r1 (singleton k2 a2)
          | otherwise      -> Empty
        Bin p2 m2 l2 r2
          | shorter m1 m2 ->
              if match p2 p1 m1
              then if zero p2 m1
                   then intersection l1 (NonEmptyMap hc2)
                   else intersection r1 (NonEmptyMap hc2)
              else Empty
          | shorter m2 m1 ->
              if match p1 p2 m2
              then if zero p1 m2
                   then intersection (NonEmptyMap (hashCons t1)) l2
                   else intersection (NonEmptyMap (hashCons t1)) r2
              else Empty
          | p1 == p2      -> let l = intersection l1 l2
                                 r = intersection r1 r2
                             in combine p1 m1 l r
          | otherwise     -> Empty

-- | /O(n)/. Difference of two maps. Keys in the first map but not in the second are included.
difference :: Hashable a => WordMap a -> WordMap a -> WordMap a
difference Empty _ = Empty
difference t1 Empty = t1
difference t1@(NonEmptyMap hc1) t2@(NonEmptyMap hc2)
  | hc1 == hc2 = Empty  -- O(1) comparison via 'HashCons'
difference (NonEmptyMap hc1) t2 =
    differenceNonEmpty (unHashCons hc1) t2

differenceNonEmpty :: Hashable a => NonEmptyWordMap a -> WordMap a -> WordMap a
differenceNonEmpty t1 Empty = NonEmptyMap (hashCons t1)
differenceNonEmpty t1 (NonEmptyMap hc2) =
    case t1 of
      Leaf k1 v1 -> if member k1 (NonEmptyMap hc2)
                    then Empty
                    else singleton k1 v1
      Bin p1 m1 l1 r1 -> case unHashCons hc2 of
        Leaf k2 a2
          | match k2 p1 m1 -> if zero k2 m1
                               then combine p1 m1 (difference l1 (singleton k2 a2)) r1
                               else combine p1 m1 l1 (difference r1 (singleton k2 a2))
          | otherwise      -> NonEmptyMap (hashCons t1)
        Bin p2 m2 l2 r2
          | shorter m1 m2 ->
              if match p2 p1 m1
              then if zero p2 m1
                   then combine p1 m1 (difference l1 (NonEmptyMap hc2)) r1
                   else combine p1 m1 l1 (difference r1 (NonEmptyMap hc2))
              else NonEmptyMap (hashCons t1)
          | shorter m2 m1 ->
              if match p1 p2 m2
              then if zero p1 m2
                   then difference (NonEmptyMap (hashCons t1)) l2
                   else difference (NonEmptyMap (hashCons t1)) r2
              else NonEmptyMap (hashCons t1)
          | p1 == p2      -> let l = difference l1 l2
                                 r = difference r1 r2
                             in combine p1 m1 l r
          | otherwise     -> NonEmptyMap (hashCons t1)

-- | Check if a key is present in the map.
member :: Word -> WordMap a -> Bool
member _ Empty = False
member k (NonEmptyMap hc) = memberNonEmpty k (unHashCons hc)

memberNonEmpty :: Word -> NonEmptyWordMap a -> Bool
memberNonEmpty k (Leaf k' _)
  | k == k'   = True
  | otherwise = False
memberNonEmpty k (Bin p m l r)
  | match k p m = if zero k m then member k l else member k r
  | otherwise   = False

-- | Helper function to combine two subtrees.
combine :: Hashable a => Word -> Word -> WordMap a -> WordMap a -> WordMap a
combine _ _ Empty Empty = Empty
combine p m l r = NonEmptyMap (hashCons (Bin p m l r))

-- | Helper functions.

join :: Hashable a => Word -> NonEmptyWordMap a -> Word -> NonEmptyWordMap a -> NonEmptyWordMap a
join p1 t1 p2 t2 =
  let m = branchingBit p1 p2
      p = mask p1 m
  in if zero p1 m
     then Bin p m (NonEmptyMap (hashCons t1)) (NonEmptyMap (hashCons t2))
     else Bin p m (NonEmptyMap (hashCons t2)) (NonEmptyMap (hashCons t1))

match :: Word -> Word -> Word -> Bool
match k p m = mask k m == p

mask :: Word -> Word -> Word
mask k m = k .&. (complement (m - 1) `xor` m)

zero :: Word -> Word -> Bool
zero k m = (k .&. m) == 0

branchingBit :: Word -> Word -> Word
branchingBit p1 p2 = highestBitMask (p1 `xor` p2)

highestBitMask :: Word -> Word
highestBitMask x
  | x == 0    = 0
  | otherwise = bit (finiteBitSize x - 1 - countLeadingZeros x)

shorter :: Word -> Word -> Bool
shorter m1 m2 = m1 > m2
