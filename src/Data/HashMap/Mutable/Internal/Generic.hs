{-# LANGUAGE RoleAnnotations #-}

module Data.HashMap.Mutable.Internal.Generic where

import Control.Monad (forM_, when)
import Control.Monad.Primitive
import Control.Monad.ST (runST)
import Data.Bits ((.&.))
import Data.Functor ((<&>))
import Data.HashMap.Mutable.Internal.Primes qualified as Primes
import Data.Hashable (Hashable)
import Data.Hashable qualified as Hashable
import Data.Primitive (Prim)
import Data.Primitive.Contiguous qualified as Array
import Data.Primitive.Contiguous.Class qualified as Array
import Data.Primitive.MutVar
import Data.Primitive.PrimArray (MutablePrimArray, sizeofMutablePrimArray)
import Data.Proxy (Proxy)
import GHC.Exts qualified as Exts

type role HashMap nominal nominal nominal nominal

newtype HashMap arr s k v = HashMap {var :: MutVar s (HashMap_ arr s k v)}

data HashMap_ arr s k v = -- | Invariants: buckets, hashes, links, keys, and values must all be the same size
  -- A value with the same index in hashes, links, keys, and values forms an entry. The index is called the entryIndex.
  -- Entries are only valid from indices 0..size, exclusive on size
  -- TODO: use one array for the control stuff
  HashMap_
  { -- | Invariant: Must be of length 3
    refs :: !(MutablePrimArray s Int),
    -- | A mapping from the hash code modulo the buckets size to an entryIndex
    -- code style, use this array whenever you want to query the size of the HashMap
    buckets :: !(MutablePrimArray s Int),
    -- | Invariant: The hash code for a particular entryIndex is set to -1 to signify a deleted element
    -- This is needed when iterating over the map, when we go through the entries instead of buckets
    hashes :: !(MutablePrimArray s Hash),
    -- | Invariant: The link for a particular entryIndex is -1 when there is no link, the end of the linked list of indices
    links :: !(MutablePrimArray s Int),
    keys :: !((Array.UnliftedMut arr) s k),
    values :: !((Array.UnliftedMut arr) s v)
  }

type role HashMap_ nominal nominal nominal nominal

type BothElement arr k v = (Array.Element arr k, Array.Element arr v)

type HasArray arr k v = (BothElement arr k v, Array.ContiguousU arr)

sizeRef, freeSizeRef, freeListRef :: Int
sizeRef = 0
freeSizeRef = 1
freeListRef = 2

type Hash = Int

defaultCapacity :: Int
defaultCapacity = 10

new :: (HasArray arr k v, PrimMonad m) => m (HashMap arr (PrimState m) k v)
new = Exts.inline newWithCapacity defaultCapacity
{-# INLINEABLE new #-}

newWithCapacity :: (HasArray arr k v, PrimMonad m) => Int -> m (HashMap arr (PrimState m) k v)
newWithCapacity capacity = newWithCapacity_ capacity >>= newMutVar <&> HashMap
{-# INLINEABLE newWithCapacity #-}

newWithCapacity_ :: (HasArray arr k v, PrimMonad m) => Int -> m (HashMap_ arr (PrimState m) k v)
newWithCapacity_ capacity = do
  let capacity' = Primes.getPrime capacity
  refs <- Array.replicateMut 3 0
  Array.write refs freeListRef -1
  buckets <- Array.replicateMut capacity' -1
  hashes <- Array.new capacity'
  links <- Array.new capacity'
  keys <- Array.new capacity'
  values <- Array.new capacity'
  pure HashMap_ {refs, buckets, hashes, links, keys = Array.unliftMut keys, values = Array.unliftMut values}
{-# INLINE newWithCapacity_ #-}

capacity_ :: HashMap_ arr s k v -> Int
capacity_ HashMap_ {buckets} = sizeofMutablePrimArray buckets
{-# INLINE capacity_ #-}

size_ :: PrimMonad m => HashMap_ arr (PrimState m) k v -> m Int
size_ HashMap_ {refs} = Array.read refs sizeRef
{-# INLINE size_ #-}

fromList :: (HasArray arr k v, PrimMonad m, Hashable k) => [(k, v)] -> m (HashMap arr (PrimState m) k v)
fromList xs = do
  map <- new
  forM_ xs $ \(k, v) -> insert k v map
  pure map
{-# INLINEABLE fromList #-}

toList :: (HasArray arr k v, PrimMonad m) => HashMap arr (PrimState m) k v -> m [(k, v)]
toList HashMap {var} = do
  HashMap_ {refs, hashes, keys, values} <- readMutVar var
  let go !i xs
        | i < 0 = pure xs
        | otherwise = do
            hashCode <- Array.read hashes i
            if hashCode == -1
              then go (i - 1) xs
              else do
                key <- Array.read (Array.liftMut keys) i
                value <- Array.read (Array.liftMut values) i
                go (i - 1) ((key, value) : xs)
  size <- Array.read refs sizeRef
  go (size - 1) []
{-# INLINEABLE toList #-}

unsafeToList :: (HasArray arr k v, PrimBase m) => HashMap arr (PrimState m) k v -> m [(k, v)]
unsafeToList HashMap {var} = do
  HashMap_ {refs, hashes, keys, values} <- readMutVar var
  let go !i
        | i < 0 = pure []
        | otherwise = do
            hashCode <- Array.read hashes i
            if hashCode == -1
              then go $ i - 1
              else do
                key <- Array.read (Array.liftMut keys) i
                value <- Array.read (Array.liftMut values) i
                xs <- unsafeInterleave $ go $ i - 1
                pure $ (key, value) : xs
  size <- Array.read refs sizeRef
  go (size - 1)
{-# INLINEABLE unsafeToList #-}

lookup :: (HasArray arr k v, PrimMonad m, Hashable k) => k -> HashMap arr (PrimState m) k v -> m (Maybe v)
lookup key = Exts.inline lookup' (Hashable.hash key) key
{-# INLINE lookup #-}

lookup' :: (HasArray arr k v, PrimMonad m, Eq k) => Hash -> k -> HashMap arr (PrimState m) k v -> m (Maybe v)
lookup' hash key HashMap {var} = do
  map@HashMap_ {values} <- readMutVar var
  i <- lookupIndex_ hash key map
  if i == -1
    then pure Nothing
    else Just <$> Array.read (Array.liftMut values) i
{-# INLINEABLE lookup' #-}

member :: (HasArray arr k v, PrimMonad m, Hashable k) => k -> HashMap arr (PrimState m) k v -> m Bool
member k = Exts.inline member' (Hashable.hash k) k
{-# INLINE member #-}

member' :: (HasArray arr k v, PrimMonad m, Eq k) => Hash -> k -> HashMap arr (PrimState m) k v -> m Bool
member' hash key HashMap {var} = do
  i <- lookupIndex_ hash key =<< readMutVar var
  pure $ i == -1
{-# INLINEABLE member' #-}

lookupIndex_ :: (HasArray arr k v, PrimMonad m, Eq k) => Hash -> k -> HashMap_ arr (PrimState m) k v -> m Int
lookupIndex_ hash key HashMap_ {buckets, hashes, keys, links} = do
  let hashCode = hash .&. hashMask
      bucketIndex = hashCode `rem` bucketsSize buckets
      go i
        | i /= -1 = do
            hashCode' <- Array.read hashes i
            if hashCode == hashCode'
              then do
                key' <- Array.read (Array.liftMut keys) i
                if key == key'
                  then pure i
                  else go =<< Array.read links i
              else go =<< Array.read links i
        | otherwise = pure -1
  go =<< Array.read buckets bucketIndex
{-# INLINEABLE lookupIndex_ #-}

delete :: (HasArray arr k v, PrimMonad m, Hashable k) => k -> HashMap arr (PrimState m) k v -> m ()
delete key = Exts.inline delete' (Hashable.hash key) key
{-# INLINEABLE delete #-}

delete' :: (HasArray arr k v, PrimMonad m, Eq k) => Hash -> k -> HashMap arr (PrimState m) k v -> m ()
delete' hash key HashMap {var} = do
  HashMap_ {refs, buckets, hashes, links, keys, values} <- readMutVar var
  let hashCode = hash .&. hashMask
      bucketIndex = hashCode `rem` bucketsSize buckets
      go prevIndex entryIndex
        | entryIndex /= -1 = do
            hashCode' <- Array.read hashes entryIndex
            if hashCode == hashCode'
              then do
                key' <- Array.read (Array.liftMut keys) entryIndex
                if key == key'
                  then do
                    next <- Array.read links entryIndex
                    if prevIndex == -1
                      then Array.write buckets bucketIndex next
                      else Array.write links prevIndex next
                    Array.write hashes entryIndex -1
                    Array.write links entryIndex =<< Array.read refs freeListRef
                    Array.write (Array.liftMut keys) entryIndex undefinedElem
                    Array.write (Array.liftMut values) entryIndex undefinedElem
                    Array.write refs freeListRef entryIndex
                    Array.write refs freeSizeRef =<< (+ 1) <$> Array.read refs freeSizeRef
                  else go entryIndex =<< Array.read links entryIndex
              else go entryIndex =<< Array.read links entryIndex
        | otherwise = pure ()
  go -1 =<< Array.read buckets bucketIndex
{-# INLINEABLE delete' #-}

insert :: (HasArray arr k v, PrimMonad m, Hashable k) => k -> v -> HashMap arr (PrimState m) k v -> m ()
insert key = Exts.inline insert' (Hashable.hash key) key
{-# INLINEABLE insert #-}

insert' :: (HasArray arr k v, PrimMonad m, Eq k) => Hash -> k -> v -> HashMap arr (PrimState m) k v -> m ()
insert' hash key value map@HashMap {var} = Exts.inline insert_' hash key value map =<< readMutVar var
{-# INLINEABLE insert' #-}

insert_' :: (HasArray arr k v, PrimMonad m, Eq k) => Hash -> k -> v -> HashMap arr (PrimState m) k v -> HashMap_ arr (PrimState m) k v -> m ()
insert_' hash key value HashMap {var} map@HashMap_ {refs, buckets, hashes, links, keys, values} = do
  let !hashCode = hash .&. hashMask
      !bucketIndex = hashCode `rem` bucketsSize buckets
      go entryIndex
        | entryIndex /= -1 = do
            hashCode' <- Array.read hashes entryIndex
            if hashCode == hashCode'
              then do
                key' <- Array.read (Array.liftMut keys) entryIndex
                if key == key'
                  then Array.write (Array.liftMut values) entryIndex value
                  else go =<< Array.read links entryIndex
              else go =<< Array.read links entryIndex
        | otherwise = addOrResize
      addOrResize = do
        freeSize <- Array.read refs freeSizeRef
        if freeSize > 0
          then do
            freeList <- Array.read refs freeListRef
            next <- Array.read links freeList
            Array.write refs freeListRef next
            Array.write refs freeSizeRef $! freeSize - 1
            add' freeList bucketIndex map
          else do
            size <- Array.read refs sizeRef
            Array.write refs sizeRef $! size + 1
            if size == bucketsSize buckets
              then do
                let newSize = Primes.getPrime $ size * 2
                map' <- resize_ newSize map
                add' size (hashCode `rem` newSize) map'
                writeMutVar var map'
              else add' size bucketIndex map
      {-# INLINE addOrResize #-}
      add' entryIndex bucketIndex map = add entryIndex bucketIndex hashCode key value map
      {-# INLINE add' #-}
  go =<< Array.read buckets bucketIndex
{-# INLINEABLE insert_' #-}

add :: (HasArray arr k v, PrimMonad m) => Int -> Int -> Hash -> k -> v -> HashMap_ arr (PrimState m) k v -> m ()
add entryIndex bucketIndex hashCode key value HashMap_ {buckets, hashes, links, keys, values} = do
  collidedEntryIndex <- Array.read buckets bucketIndex
  Array.write hashes entryIndex hashCode
  Array.write links entryIndex collidedEntryIndex
  Array.write (Array.liftMut keys) entryIndex key
  Array.write (Array.liftMut values) entryIndex value
  Array.write buckets bucketIndex entryIndex
{-# INLINE add #-}

resize_ :: (HasArray arr k v, PrimMonad m) => Int -> HashMap_ arr (PrimState m) k v -> m (HashMap_ arr (PrimState m) k v)
resize_ newCapacity map@HashMap_ {buckets, hashes, links, keys, values} = do
  buckets' <- Array.replicateMut newCapacity -1
  hashes' <- Array.resize hashes newCapacity
  links' <- Array.resize links newCapacity
  keys' <- Array.new newCapacity
  values' <- Array.new newCapacity
  bucketSize <- Array.sizeMut buckets
  Array.copyMut keys' 0 $ Array.sliceMut (Array.liftMut keys) 0 bucketSize
  Array.copyMut values' 0 $ Array.sliceMut (Array.liftMut values) 0 bucketSize

  let go entryIndex
        | entryIndex < bucketsSize buckets = do
            hashCode <- Array.read hashes' entryIndex
            when (hashCode /= -1) $ do
              let bucketIndex = hashCode `rem` newCapacity
              collidedEntryIndex <- Array.read buckets' bucketIndex
              Array.write links' entryIndex collidedEntryIndex
              Array.write buckets' bucketIndex entryIndex
            go $ entryIndex + 1
        | otherwise = pure ()
  go 0

  pure
    map
      { buckets = buckets',
        hashes = hashes',
        links = links',
        keys = Array.unliftMut keys',
        values = Array.unliftMut values'
      }
{-# INLINE resize_ #-}

nubHash :: (HasArray arr v (), Hashable v) => Proxy arr -> [v] -> [v]
nubHash p = nubHashOn p id
{-# INLINE nubHash #-}

nubHashOn :: (HasArray arr k (), Hashable k) => Proxy arr -> (v -> k) -> [v] -> [v]
nubHashOn p on xs = Exts.build $ \c n -> nubHashOnWith p on c n xs
{-# INLINE nubHashOn #-}

nubHashOnWith :: forall arr k v r. (HasArray arr k (), Hashable k) => Proxy arr -> (v -> k) -> (v -> r -> r) -> r -> [v] -> r
nubHashOnWith _ on c n xs = runST $ do
  m <- new @arr
  let go [] = pure n
      go (x : xs) = do
        let k = on x
        mem <- member k m
        if mem
          then go xs
          else do
            insert k () m
            r <- unsafeInterleave $ go xs
            pure $ c x r
  go xs
{-# INLINE nubHashOnWith #-}

bucketsSize :: Prim a => MutablePrimArray s a -> Int
bucketsSize = sizeofMutablePrimArray
{-# INLINE bucketsSize #-}

undefinedElem :: forall a. a
undefinedElem = error "Undefined element"
{-# NOINLINE undefinedElem #-}

hashMask :: Int
hashMask = maxBound
