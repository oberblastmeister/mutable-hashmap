{-# LANGUAGE MagicHash #-}

module Data.HashMap.Mutable.Internal.Utils where

import Data.Bits qualified as Bits
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
