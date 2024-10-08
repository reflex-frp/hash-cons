module Data.HashCons.Internal.Test (tests) where

import Control.Concurrent.Async
import Control.Monad
import Data.HashCons.Internal
import Data.Hashable
import Data.IORef
import System.Mem.StableName
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Data.HashCons.Internal"
  [ QC.testProperty "Reflexivity" prop_reflexivity
  , QC.testProperty "Symmetry" prop_symmetry
  , QC.testProperty "Consistency of unHashCons" prop_consistency_unHashCons
  , QC.testProperty "Deduplication" prop_deduplication
  , QC.testProperty "Ord and Eq Consistency" prop_ord_eq_consistency
  , QC.testProperty "Ord Transitivity" prop_ord_transitivity
  ]

prop_reflexivity :: Int -> Property
prop_reflexivity x = let hx = hashCons x in hx === hx

prop_symmetry :: Int -> Int -> Property
prop_symmetry x y = let hx = hashCons x; hy = hashCons y in (hx == hy) === (hy == hx)

prop_consistency_unHashCons :: Int -> Int -> Property
prop_consistency_unHashCons x y = let hx = hashCons x; hy = hashCons y in
  (hx == hy) === (unHashCons hx == unHashCons hy)

prop_deduplication :: Int -> Int -> Property
prop_deduplication x y = ioProperty $ do
  let hx = hashCons x
      hy = hashCons y
  -- Perform the equality check to trigger deduplication
  areEqual <- eqHashConsIO hx hy
  if areEqual
    then do
      -- Read the contents of both IORefs
      a1 <- readIORef (_hashCons_ref hx)
      a2 <- readIORef (_hashCons_ref hy)
      -- Create StableNames for both contents
      sn1 <- makeStableName a1
      sn2 <- makeStableName a2
      -- Check if the StableNames are equal (pointer equality)
      return $ sn1 == sn2
    else return True -- If not equal, deduplication isn't expected

-- Helper function to perform equality check in IO
eqHashConsIO :: Eq a => HashCons a -> HashCons a -> IO Bool
eqHashConsIO (HashConsC h1 ref1) (HashConsC h2 ref2)
  | h1 /= h2     = return False
  | ref1 == ref2 = return True
  | otherwise    = do
      a1 <- readIORef ref1
      a2 <- readIORef ref2
      if a1 == a2
        then do
          writeIORef ref1 a2
          return True
        else return False

prop_ord_eq_consistency :: Int -> Int -> Bool
prop_ord_eq_consistency x y = let hx = hashCons x; hy = hashCons y in
  (hx == hy) == (compare hx hy == EQ)

prop_ord_transitivity :: Int -> Int -> Int -> Property
prop_ord_transitivity x y z = let hx = hashCons x; hy = hashCons y; hz = hashCons z in
  (compare hx hy == LT && compare hy hz == LT) ==> (compare hx hz == LT)
