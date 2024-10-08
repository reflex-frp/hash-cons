module Data.HashCons.IntMap.Test (tests) where

import Prelude hiding (lookup)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Maybe (isJust, isNothing)
import Data.HashCons.IntMap
import Data.Hashable

tests :: TestTree
tests = testGroup "Data.HashCons.IntMap"
  [ QC.testProperty "Insert and Lookup" prop_insertLookup
  , QC.testProperty "Insert overwrites existing key" prop_insertOverwrite
  , QC.testProperty "Insert preserves other keys" prop_insertPreservesOtherKeys
  , QC.testProperty "Union contains all keys" prop_unionContainsAllKeys
  , QC.testProperty "Intersection contains only common keys" prop_intersectionCommonKeys
  , QC.testProperty "Difference removes keys" prop_differenceRemovesKeys
  ]

-- Property: Inserting a key and then looking it up gives back the value
prop_insertLookup :: Int -> Int -> Bool
prop_insertLookup k v =
  lookup k (insert k v empty) == Just v

-- Property: Inserting a key overwrites the existing value
prop_insertOverwrite :: Int -> Int -> Int -> Bool
prop_insertOverwrite k v1 v2 =
  lookup k (insert k v2 (insert k v1 empty)) == Just v2

-- Property: Inserting a key does not affect other keys
prop_insertPreservesOtherKeys :: Int -> Int -> Int -> Int -> Property
prop_insertPreservesOtherKeys k1 v1 k2 v2 =
  k1 /= k2 ==> lookup k1 (insert k2 v2 (insert k1 v1 empty)) == Just v1

-- Property: Union contains all keys from both maps
prop_unionContainsAllKeys :: [(Int, Int)] -> [(Int, Int)] -> Int -> Bool
prop_unionContainsAllKeys kvs1 kvs2 k =
  let m1 = fromList kvs1
      m2 = fromList kvs2
      mu = union m1 m2
  in lookup k mu == case lookup k m1 of
                      Just v  -> Just v
                      Nothing -> lookup k m2

-- Property: Intersection contains only keys common to both maps
prop_intersectionCommonKeys :: [(Int, Int)] -> [(Int, Int)] -> Int -> Bool
prop_intersectionCommonKeys kvs1 kvs2 k =
  let m1 = fromList kvs1
      m2 = fromList kvs2
      mi = intersection m1 m2
  in lookup k mi == if isJust (lookup k m1) && isJust (lookup k m2) then lookup k m1 else Nothing

-- Property: Difference removes keys present in the second map from the first map
prop_differenceRemovesKeys :: [(Int, Int)] -> [(Int, Int)] -> Int -> Bool
prop_differenceRemovesKeys kvs1 kvs2 k =
  let m1 = fromList kvs1
      m2 = fromList kvs2
      md = difference m1 m2
  in lookup k md == if isJust (lookup k m1) && isNothing (lookup k m2) then lookup k m1 else Nothing

-- Helper function to create a map from a list of key-value pairs
fromList :: Hashable a => [(Int, a)] -> IntMap a
fromList = foldr (\(k,v) m -> insert k v m) empty
