-- Generated file, do not edit!

{-# LANGUAGE StarIsType #-}
module Data.HashMap.Mutable.Boxed(
fromList
)
where

fromList :: forall (k :: *) (v :: *) (m :: * ->
                                           *) . (HasArray Array k v, PrimMonad m, Hashable k) =>
            [(k, v)] -> m (HashMap Array (PrimState m) k v) = fromList
