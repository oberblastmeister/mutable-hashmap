{-# LANGUAGE MagicHash #-}

module Data.HashMap.Mutable.Internal.Basic where

import Control.Monad (forM_, (<$!>))
import Control.Monad.Primitive
import Data.Bits ((.&.))
import Data.Bits qualified as Bits
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.HashMap.Mutable.Internal.Utils
import Data.Hashable (Hashable)
import Data.Hashable qualified as Hashable
import Data.Primitive.Array
import Data.Primitive.MutVar
import Data.Word (Word64)
import GHC.Exts qualified as Exts
import GHC.Stack (HasCallStack)

loadFactor :: Word64
loadFactor = 750

loadFactorDenum :: Word64
loadFactorDenum = 1000

newtype HashMap s k v = HashMap (MutVar s (HashMap_ s k v))

data HashMap_ s k v = HashMap_
  { size :: !Int,
    capacity :: !Int,
    buckets :: !(MutableArray s [Entry k v])
  }

data Entry k v = Entry
  { key :: !k,
    value :: v,
    hash :: !Hash
  }

type Hash = Int

defaultCapacity :: Int
defaultCapacity = 16

new :: PrimMonad m => m (HashMap (PrimState m) k v)
new = Exts.inline newWithCapacity defaultCapacity
{-# INLINEABLE new #-}

newWithCapacity :: (HasCallStack, PrimMonad m) => Int -> m (HashMap (PrimState m) k v)
newWithCapacity bucketsCap = new_ bucketsCap >>= newMutVar <&> HashMap
{-# INLINEABLE newWithCapacity #-}

fromList :: (PrimMonad m, Hashable k) => [(k, v)] -> m (HashMap (PrimState m) k v)
fromList xs = do
  map <- new
  forM_ xs $ \(k, v) -> insert k v map
  pure map
{-# INLINEABLE fromList #-}

new_ :: (HasCallStack, PrimMonad m) => Int -> m (HashMap_ (PrimState m) k v)
new_ bucketsCap
  | bucketsCap < 0 = error "Initial capacity must not be negative"
  | otherwise = do
      buckets <- newArray (nextPowerOfTwo bucketsCap) []
      pure $! HashMap_ {buckets, size = 0, capacity = newCapacity bucketsCap}
{-# INLINE new_ #-}

lookup :: (PrimMonad m, Hashable k) => k -> HashMap (PrimState m) k v -> m (Maybe v)
lookup k (HashMap var) = do
  HashMap_ {buckets} <- readMutVar var
  let h = Hashable.hash k
      i = getIndex buckets h
  bucket <- readArray buckets i
  pure $! value <$!> find (bucketMatches h k) bucket
{-# INLINEABLE lookup #-}

insert :: (PrimMonad m, Hashable k) => k -> v -> HashMap (PrimState m) k v -> m ()
insert k v (HashMap var) = do
  map@HashMap_ {size, capacity, buckets} <- readMutVar var
  let !h = Hashable.hash k
      !i = getIndex buckets h
  bucket <- readArray buckets i
  let !bucket' = Entry {hash = h, key = k, value = v} : findDeleteList (bucketMatches h k) bucket
      !size' = size + 1
  writeArray buckets i bucket'
  let !map' = map {size = size'}
  ( if size' > capacity
      then resize_ (sizeofMutableArray buckets !<<. 1) map'
      else pure map'
    )
    >>= writeMutVar var
{-# INLINEABLE insert #-}

bucketMatches :: Eq k => Hash -> k -> Entry k v -> Bool
bucketMatches h k Entry {key, hash} = hash == h && key == k
{-# INLINE bucketMatches #-}

findDeleteList :: (a -> Bool) -> [a] -> [a]
findDeleteList f = go
  where
    go [] = []
    go (x : xs) = if f x then xs else x : go xs
{-# INLINE findDeleteList #-}

resize_ :: PrimMonad m => Int -> HashMap_ (PrimState m) k v -> m (HashMap_ (PrimState m) k v)
resize_ bucketsCap map@HashMap_ {buckets} = do
  buckets' <- newArray bucketsCap []
  let go i
        | i < 0 = pure ()
        | otherwise = do
            bucket <- readArray buckets i
            forM_ bucket $ \entry@Entry {hash} -> do
              let j = getIndex buckets' hash
              writeArray buckets' j =<< ((entry :) <$!> readArray buckets' j)
            go $ i - 1
  go $ sizeofMutableArray buckets - 1
  pure $! map {buckets = buckets', capacity = newCapacity bucketsCap}
{-# INLINE resize_ #-}

resizeIO :: Int -> HashMap_ (PrimState IO) k v -> IO (HashMap_ (PrimState IO) k v)
resizeIO = undefined

-- Returns a power of two size for the given target capacity.
nextPowerOfTwo :: Int -> Int
nextPowerOfTwo cap = if n < 0 then 1 else n + 1
  where
    n = -1 !>>>. Bits.countLeadingZeros (cap - 1)
{-# INLINE nextPowerOfTwo #-}

newCapacity :: Int -> Int
newCapacity capacity = fromIntegral $ ((fromIntegral capacity :: Word64) * loadFactor) `div` loadFactorDenum
{-# INLINE newCapacity #-}

getIndex :: MutableArray s a -> Hash -> Int
getIndex marr h = improve h .&. (sizeofMutableArray marr - 1)
{-# INLINE getIndex #-}

improve :: Hash -> Hash
improve h = h `Bits.xor` (h !>>>. (intSize !>>>. 1))
{-# INLINE improve #-}

intSize :: Int
intSize = Bits.finiteBitSize @Int undefined

undefinedElem :: forall a. a
undefinedElem = error "Undefined element"
