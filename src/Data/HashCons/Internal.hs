{-# LANGUAGE MagicHash #-}

module Data.HashCons.Internal where

import Control.Monad (when)
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Text.ParserCombinators.ReadPrec (step)
import Text.Read (Read(..), lexP, parens, prec)
import Text.Read.Lex (Lexeme (Ident))
import GHC.Exts (reallyUnsafePtrEquality#)

-- | 'HashCons' with a precomputed hash and an 'IORef' to the value.
--
-- WARNING: Do not use this type to wrap types whose Eq or Ord instances
-- allow distinguishable values to compare as equal; this will result in
-- nondeterminism or even visible mutation of semantically-immutable
-- values at runtime.
data HashCons a = HashCons
  { _hashCons_hash :: {-# UNPACK #-} !Int       -- ^ Precomputed hash
  , _hashCons_ref  :: {-# UNPACK #-} !(IORef a) -- ^ Reference to the value
  }

-- | Create a new 'HashCons'.
hashCons :: Hashable a => a -> HashCons a
hashCons a = HashCons (hash a) $ unsafeDupablePerformIO $ newIORef a

-- | Extract the value from a 'HashCons'.
unHashCons :: HashCons a -> a
unHashCons (HashCons _ ref) = unsafeDupablePerformIO $ readIORef ref

-- | Show instance that displays 'HashCons' in the format "hashCons <x>"
instance Show a => Show (HashCons a) where
    showsPrec d hc = showParen (d > appPrec) $
        showString "hashCons " . showsPrec (appPrec + 1) (unHashCons hc)
      where
        appPrec = 10

-- | Read instance that parses 'HashCons' from the format "hashCons <x>"
instance (Read a, Hashable a) => Read (HashCons a) where
  readPrec = parens $ prec 10 $ do
    Ident "hashCons" <- lexP
    a <- step readPrec
    pure $ hashCons a

instance Eq a => Eq (HashCons a) where
  HashCons h1 ref1 == HashCons h2 ref2
    | ref1 == ref2 = True
    | h1 /= h2 = False
    | otherwise = unsafeDupablePerformIO $ do
        a1 <- readIORef ref1
        a2 <- readIORef ref2
        let eq = a1 == a2
        when eq $ case reallyUnsafePtrEquality# a1 a2 of
          0# -> writeIORef ref1 a2
          _ -> pure ()
        pure eq

-- | NOTE: This instance orders by hash first, and only secondarily by
-- the 'Ord' instance of 'a', to improve performance.
instance Ord a => Ord (HashCons a) where
  compare (HashCons h1 ref1) (HashCons h2 ref2) = case compare h1 h2 of
    EQ -> if ref1 == ref2
      then EQ
      else unsafeDupablePerformIO $ do
        a1 <- readIORef ref1
        a2 <- readIORef ref2
        let result = compare a1 a2
        when (result == EQ) $ case reallyUnsafePtrEquality# a1 a2 of
          0# -> writeIORef ref1 a2
          _ -> pure ()
        pure result
    result -> result

instance Eq a => Hashable (HashCons a) where
  hashWithSalt salt (HashCons h _) = hashWithSalt salt h
