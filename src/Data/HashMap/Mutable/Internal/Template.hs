-- Generated file, do not edit!
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Data.HashMap.Mutable.MOD
  ( HashMap,
    new,
    newWithCapacity,
    delete,
    lookup,
    toList,
    fromList,
    insert,
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.HashMap.Mutable.Generic (HasArray)
import Data.HashMap.Mutable.Generic qualified as Generic
import Data.Hashable (Hashable)
import Data.Primitive qualified as Primitive
import Data.Primitive.Contiguous qualified as Contiguous
import Prelude hiding (lookup)

#ifndef ARR1
-- this is just for better ide support
#define ARR1 Primitive.Array
#endif

#ifndef ARR2
#define ARR2 Primitive.Array
#endif

type HashMap = Generic.HashMap ARR1 ARR2

new :: (HasArray ARR1 k, HasArray ARR2 v, PrimMonad m) => m (HashMap (PrimState m) k v)
new = Generic.new
{-# INLINE new #-}

newWithCapacity :: (HasArray ARR1 k, HasArray ARR2 v, PrimMonad m) => Int -> m (HashMap (PrimState m) k v)
newWithCapacity = Generic.newWithCapacity
{-# INLINE newWithCapacity #-}

delete :: (HasArray ARR1 k, HasArray ARR2 v, PrimMonad m, Hashable k) => k -> HashMap (PrimState m) k v -> m ()
delete = Generic.delete
{-# INLINE delete #-}

lookup :: (HasArray ARR1 k, HasArray ARR2 v, PrimMonad m, Hashable k) => k -> HashMap (PrimState m) k v -> m (Maybe v)
lookup = Generic.lookup
{-# INLINE lookup #-}

toList :: (HasArray ARR1 k, HasArray ARR2 v, PrimMonad m) => HashMap (PrimState m) k v -> m [(k, v)]
toList = Generic.toList
{-# INLINE toList #-}

fromList :: (HasArray ARR1 k, HasArray ARR2 v, PrimMonad m, Hashable k) => [(k, v)] -> m (HashMap (PrimState m) k v)
fromList = Generic.fromList
{-# INLINE fromList #-}

insert :: (HasArray ARR1 k, HasArray ARR2 v, PrimMonad m, Hashable k) => k -> v -> HashMap (PrimState m) k v -> m ()
insert = Generic.insert
{-# INLINE insert #-}
