module Main (main) where

import Test.Tasty
import qualified Data.HashCons.Internal.Test
import qualified Data.HashCons.IntMap.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "hash-cons"
  [ Data.HashCons.Internal.Test.tests
  , Data.HashCons.IntMap.Test.tests
  ]
