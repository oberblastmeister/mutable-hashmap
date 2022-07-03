{-# LANGUAGE MagicHash #-}

module Data.HashMap.Mutable.Internal.Utils where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits qualified as Bits
import Data.Primitive.Array (Array)
import Data.Primitive.Contiguous (Contiguous, UnliftedArray)
import Data.Primitive.Contiguous qualified as Contiguous
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.SmallArray (SmallArray)
import GHC.Exts (Int (I#))
import GHC.Exts qualified as Exts

(!<<.) :: Int -> Int -> Int
(!<<.) = Bits.unsafeShiftL
{-# INLINE (!<<.) #-}

(!>>>.) :: Int -> Int -> Int
(!>>>.) = uncheckedIShiftRL
{-# INLINE (!>>>.) #-}

(>>=!) :: Monad m => m t -> (t -> m b) -> m b
(>>=!) m f = do !x <- m; f x
{-# INLINE (>>=!) #-}

uncheckedIShiftRL :: Int -> Int -> Int
uncheckedIShiftRL (I# n) (I# i) = I# (Exts.uncheckedIShiftRL# n i)
{-# INLINE uncheckedIShiftRL #-}

class Contiguous arr => DeleteEntry arr where
  deleteEntry :: (PrimMonad m) =>  (Contiguous.Mutable arr) (PrimState m) x -> Int -> m ()

instance DeleteEntry PrimArray where
  deleteEntry _ _ = pure ()

instance DeleteEntry SmallArray where
  deleteEntry arr i = Contiguous.write arr i undefinedElem

instance DeleteEntry Array where
  deleteEntry arr i = Contiguous.write arr i undefinedElem

instance DeleteEntry UnliftedArray where
  deleteEntry _ _ = pure ()

undefinedElem :: forall a. a
undefinedElem = error "Undefined element"
{-# NOINLINE undefinedElem #-}
