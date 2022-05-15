{-# LANGUAGE CPP #-}

module Data.HashMap.Mutable.Internal.PrimArray where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive (Prim)
import Data.Primitive.PrimArray
import Prelude hiding (length)

#include "mutable-hashmap.h"

read :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> m a
read marr i = CHECK_BOUNDS ("read", lengthM marr, i) readPrimArray marr i
{-# INLINE read #-}

write :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> a -> m ()
write marr i x = CHECK_BOUNDS ("write", lengthM marr, i) writePrimArray marr i x
{-# INLINE write #-}

lengthM :: Prim a => MutablePrimArray s a -> Int
lengthM = sizeofMutablePrimArray
{-# INLINE lengthM #-}

length :: Prim a => PrimArray a -> Int
length = sizeofPrimArray
{-# INLINE length #-}

replicate :: (PrimMonad m, Prim a) => Int -> a -> m (MutablePrimArray (PrimState m) a)
replicate n x = do
  xs <- new n
  setPrimArray xs 0 n x
  pure xs
{-# INLINE replicate #-}

new :: (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
new i = CHECK_GT ("new", i, 0) newPrimArray i
{-# INLINE new #-}

index :: (Prim a) => PrimArray a -> Int -> a
index arr i = CHECK_BOUNDS ("index", length arr, i) indexPrimArray arr i
{-# INLINE index #-}

resize :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> m (MutablePrimArray (PrimState m) a)
resize marr i = CHECK_GT("resize", i, 0) resizeMutablePrimArray marr i
{-# INLINE resize #-}
