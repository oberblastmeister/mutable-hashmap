{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.HashMap.Mutable.Internal.Array where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.Array

#include "mutable-hashmap.h"

read :: (PrimMonad m) => MutableArray (PrimState m) a -> Int -> m a
read marr i = CHECK_BOUNDS ("read", lengthM marr, i) readArray marr i
{-# INLINE read #-}

write :: (PrimMonad m) => MutableArray (PrimState m) a -> Int -> a -> m ()
write marr i x = CHECK_BOUNDS ("write", lengthM marr, i) writeArray marr i x
{-# INLINE write #-}

lengthM :: MutableArray s a -> Int
lengthM = sizeofMutableArray
{-# INLINE lengthM #-}

new :: (PrimMonad m) => Int -> a -> m (MutableArray (PrimState m) a)
new i x = CHECK_GT ("new", i, 0) newArray i x
{-# INLINE new #-}

{- ORMOLU_DISABLE -}
copyM :: (PrimMonad m) => MutableArray (PrimState m) e -> Int -> MutableArray (PrimState m) e -> Int -> Int -> m ()
copyM src si dst di n =
  CHECK_BOUNDS("copyM: src", lengthM src, si + n - 1)
  CHECK_BOUNDS("copyM: dst", lengthM dst, di + n - 1)
  copyMutableArray src si dst di n
{-# INLINE copyM #-}
{- ORMOLU_ENABLE -}
