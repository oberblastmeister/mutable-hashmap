{-# LANGUAGE MultiWayIf #-}

module Data.HashMap.Mutable.Internal.Robin where

import Control.Monad (unless)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Functor ((<&>))
import Data.HashMap.Mutable.Internal.Primes qualified as Primes
import Data.Hashable (Hashable)
import Data.Hashable qualified as Hashable
import Data.Primitive (MutablePrimArray)
import Data.Primitive.Contiguous.Class qualified as Array
import Data.Primitive.MutVar (MutVar)
import Data.Primitive.MutVar qualified as MutVar
import Data.Word (Word64)
import GHC.Exts qualified as Exts

newtype HashMap arr s k v = HashMap {var :: MutVar s (HashMap_ arr s k v)}

data HashMap_ arr s k v = HashMap_
  { refs :: !(MutablePrimArray s Int),
    info :: !(MutablePrimArray s Int),
    keys :: !((Array.UnliftedMut arr) s k),
    values :: !((Array.UnliftedMut arr) s v)
  }

sizeIx, thresholdIx :: Int
sizeIx = 0
thresholdIx = 1

type Hash = Int

type BothElement arr k v = (Array.Element arr k, Array.Element arr v)

type HasArray arr k v = (BothElement arr k v, Array.ContiguousU arr)

new :: (HasArray arr k v, PrimMonad m) => m (HashMap arr (PrimState m) k v)
new = Exts.inline newWithCapacity defaultCapacity
{-# INLINEABLE new #-}

defaultCapacity :: Int
defaultCapacity = 16

newWithCapacity :: (HasArray arr k v, PrimMonad m) => Int -> m (HashMap arr (PrimState m) k v)
newWithCapacity capacity = newWithCapacity_ capacity >>= MutVar.newMutVar <&> HashMap
{-# INLINEABLE newWithCapacity #-}

newWithCapacity_ :: (HasArray arr k v, PrimMonad m) => Int -> m (HashMap_ arr (PrimState m) k v)
newWithCapacity_ capacity = do
  capacity <- pure $! Primes.getPrime capacity
  refs <- Array.replicateMut 2 0
  Array.write refs thresholdIx $! getThreshold capacity
  info <- Array.replicateMut (capacity * 2) 0
  keys <- Array.new capacity
  values <- Array.new capacity
  pure HashMap_ {refs, info, keys = Array.unliftMut keys, values = Array.unliftMut values}
{-# INLINE newWithCapacity_ #-}

getThreshold :: Int -> Int
getThreshold capacity = fromIntegral $ ((fromIntegral capacity :: Word64) * 10 + 9) `div` 11
{-# INLINE getThreshold #-}

insert :: (PrimMonad m, HasArray arr k v, Hashable k) => k -> v -> HashMap arr (PrimState m) k v -> m ()
insert key value HashMap {var} = do
  map@HashMap_ {refs, keys} <- MutVar.readMutVar var
  capacity <- Array.sizeMut (Array.liftMut keys)
  size <- Array.read refs sizeIx
  threshold <- Array.read refs thresholdIx
  map <-
    if size > threshold
      then do
        let newCapacity = Primes.getPrime $! capacity * 2
        Array.write refs thresholdIx $! getThreshold newCapacity
        resize_ newCapacity map
      else pure map
  insert_ key value map
  Array.write refs sizeIx =<< (1 +) <$> Array.read refs sizeIx
{-# INLINEABLE insert #-}

insert_ :: (PrimMonad m, HasArray arr k v, Hashable k) => k -> v -> HashMap_ arr (PrimState m) k v -> m ()
insert_ key value HashMap_ {info, keys, values} = do
  capacity <- Array.sizeMut (Array.liftMut keys)
  let !hash = Hashable.hash key
      bucketIx = hash `mod` capacity
      go !bucketIx !dist !hash !key value = do
        hash' <- Array.read info (bucketIx * 2)
        dist' <- Array.read info (bucketIx * 2 + 1)
        if hash' == 0
          then do
            Array.write info (bucketIx * 2) hash
            Array.write info (bucketIx * 2 + 1) dist
            Array.write (Array.liftMut keys) bucketIx key
            Array.write (Array.liftMut values) bucketIx value
          else do
            key' <- Array.read (Array.liftMut keys) bucketIx
            if
                | hash == hash' && key == key' -> Array.write (Array.liftMut values) bucketIx value
                -- robinhood!
                | dist' < dist -> do
                    value' <- Array.read (Array.liftMut values) bucketIx
                    Array.write info (bucketIx * 2) hash
                    Array.write info (bucketIx * 2 + 1) dist
                    Array.write (Array.liftMut keys) bucketIx key
                    Array.write (Array.liftMut values) bucketIx value
                    go ((bucketIx + 1) `mod` capacity) (dist' + 1) hash' key' value'
                | otherwise -> go ((bucketIx + 1) `mod` capacity) (dist + 1) hash' key value
  go bucketIx 0 hash key value
{-# INLINE insert_ #-}

add_' :: (PrimMonad m, HasArray arr k v) => Hash -> k -> v -> HashMap_ arr (PrimState m) k v -> m ()
add_' hash key value HashMap_ {info, keys, values} = do
  capacity <- Array.sizeMut (Array.liftMut keys)
  let bucketIx = hash `mod` capacity
      go !bucketIx !dist !hash !key value = do
        hash' <- Array.read info (bucketIx * 2)
        dist' <- Array.read info (bucketIx * 2 + 1)
        if
            | hash' == 0 -> do
                Array.write info (bucketIx * 2) hash
                Array.write info (bucketIx * 2 + 1) dist
                Array.write (Array.liftMut keys) bucketIx key
                Array.write (Array.liftMut values) bucketIx value
            -- robinhood!
            | dist' < dist -> do
                value' <- Array.read (Array.liftMut values) bucketIx
                key' <- Array.read (Array.liftMut keys) bucketIx
                Array.write info (bucketIx * 2) hash
                Array.write info (bucketIx * 2 + 1) dist
                Array.write (Array.liftMut keys) bucketIx key
                Array.write (Array.liftMut values) bucketIx value
                go ((bucketIx + 1) `mod` capacity) (dist' + 1) hash' key' value'
            | otherwise -> go ((bucketIx + 1) `mod` capacity) (dist + 1) hash' key value
  go bucketIx 0 hash key value
{-# INLINE add_' #-}

resize_ :: (PrimMonad m, HasArray arr k v) => Int -> HashMap_ arr (PrimState m) k v -> m (HashMap_ arr (PrimState m) k v)
resize_ newCapacity map@HashMap_ {info, keys, values} = do
  info' <- Array.replicateMut (newCapacity * 2) 0
  keys' <- Array.new newCapacity
  values' <- Array.new newCapacity
  oldCapacity <- Array.sizeMut (Array.liftMut keys)
  let map' = map {info = info', keys = Array.unliftMut keys', values = Array.unliftMut values'}
      go i
        | i < oldCapacity = do
            hash <- Array.read info (i * 2)
            unless (hash == 0) $ do
              key <- Array.read (Array.liftMut keys) i
              value <- Array.read (Array.liftMut values) i
              add_' hash key value map'
        | otherwise = pure ()
  go 0
  pure map'
{-# INLINE resize_ #-}
