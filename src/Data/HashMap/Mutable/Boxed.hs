-- Generated file, do not edit!

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Data.HashMap.Mutable.Boxed
  ( HashMap,
    new,
    newWithCapacity,
    delete,
    lookup,
    toList,
    fromList,
  )
where

import Prelude hiding (lookup)
import Data.Hashable (Hashable)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.HashMap.Mutable.Generic qualified as Generic
import Data.HashMap.Mutable.Generic (BothElement)
import Data.Primitive qualified as Primitive
import Data.Primitive.Contiguous qualified as Contiguous

type HashMap = Generic.HashMap Primitive.Array

new :: (BothElement Primitive.Array k v, PrimMonad m) => m (HashMap (PrimState m) k v)
new = Generic.new
{-# INLINE new #-}

newWithCapacity :: (BothElement Primitive.Array k v, PrimMonad m) => Int -> m (HashMap (PrimState m) k v)
newWithCapacity = Generic.newWithCapacity
{-# INLINE newWithCapacity #-}

delete :: (BothElement Primitive.Array k v, PrimMonad m, Hashable k) => k -> HashMap (PrimState m) k v -> m ()
delete = Generic.delete
{-# INLINE delete #-}

lookup :: (BothElement Primitive.Array k v, PrimMonad m, Hashable k) => k -> HashMap (PrimState m) k v -> m (Maybe v)
lookup = Generic.lookup
{-# INLINE lookup #-}

toList :: (BothElement Primitive.Array k v, PrimMonad m) => HashMap (PrimState m) k v -> m [(k, v)]
toList = Generic.toList
{-# INLINE toList #-}

fromList :: (BothElement Primitive.Array k v, PrimMonad m, Hashable k) => [(k, v)] -> m (HashMap (PrimState m) k v)
fromList = Generic.fromList
{-# INLINE fromList #-}