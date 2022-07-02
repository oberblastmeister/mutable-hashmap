{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Random (MonadRandom)
import Control.Monad.Random.Strict qualified as Monad.Random
import Criterion.Main
import Data.Constraint (Dict (..))
import Data.Foldable qualified as Foldable
import Data.HashMap.Mutable.Internal.Generic qualified as HashMap.Mutable.Arena
import Data.HashTable.IO qualified as HashTable
import Data.Primitive (Array)
import Data.Proxy
import Data.Vector qualified as VB
import Data.Vector.Hashtables qualified as Vector.Hashtables
import Data.Vector.Hashtables.Internal qualified as VH
import Data.Vector.Mutable qualified as VBM
import Immutable.Shuffle qualified
import IsMap (IsMap)
import IsMap qualified
import System.Random (Random)

insertMonotonic :: Int -> (Int -> IO h) -> (Int -> Int -> h -> IO ()) -> IO h
insertMonotonic n new insert = do
  map <- new n
  let go i
        | i <= n = do insert i i map; go $ i + 1
        | otherwise = pure ()
  go 0
  pure map
{-# NOINLINE insertMonotonic #-}

vhfind :: Int -> VH.Dictionary (PrimState IO) VBM.MVector Int VBM.MVector Int -> IO Int
vhfind n ht = do
  let go !i !s
        | i <= n = do
            x <- VH.findEntry ht i
            go (i + 1) (s + x)
        | otherwise = return s
  go 0 0

main :: IO ()
main = do
  let inputs = [100, 1000, 10000, 100000, 1000000]
  comparisonBench <- mapM comparisonBenches inputs
  defaultMain
    [ bgroup "comparison" comparisonBench
    ]

comparisonBenches :: Int -> IO Benchmark
comparisonBenches n =
  pure $
    bgroup
      (show n)
      [ bgroup
          "insert"
          [ bench "mutable-hashmaps arena" $
              whnfIO $
                insertMonotonic
                  n
                  (HashMap.Mutable.Arena.newWithCapacity @Array)
                  HashMap.Mutable.Arena.insert,
            bench "vector-hashtables" $
              whnfIO $
                insertMonotonic
                  n
                  (Vector.Hashtables.initialize @VBM.MVector @Int @VBM.MVector @Int)
                  (\i j m -> Vector.Hashtables.insert m i j)
          ],
        bgroup
          "insert resize"
          [ bench "mutable-hashmaps arena" $
              whnfIO $
                insertMonotonic
                  n
                  (const (HashMap.Mutable.Arena.new @Array))
                  HashMap.Mutable.Arena.insert,
            bench "vector-hashtables" $
              whnfIO $
                insertMonotonic
                  n
                  (const $ Vector.Hashtables.initialize @VBM.MVector @Int @VBM.MVector @Int 10)
                  (\i j m -> Vector.Hashtables.insert m i j),
            bench "hashtables basic" $
              whnfIO $
                insertMonotonic
                  n
                  (const (HashTable.new :: IO (HashTable.BasicHashTable Int Int)))
                  (\i j m -> HashTable.insert m i j)
          ]
      ]

makeBenches :: forall map. Proxy map -> Int -> Benchmark
makeBenches _ n =
  bgroup
    []
    undefined

-- mapConstraints :: [forall map. Dict (IsMap map)]
-- mapConstraints =
--   [Dict @(IsMap Int)]

getRandomVec :: (MonadRandom m, Random a) => m (VB.Vector a)
getRandomVec = VB.fromList <$> Monad.Random.getRandoms

getRandomVecRs :: (MonadRandom m, Random a) => (a, a) -> m (VB.Vector a)
getRandomVecRs rs = VB.fromList <$> Monad.Random.getRandomRs rs

getFullInts :: MonadRandom m => Int -> m (VB.Vector Int)
getFullInts n = VB.generateM n $ \_ -> Monad.Random.getRandom

getShuffledRangeInts :: (MonadRandom m, PrimMonad m) => Int -> m (VB.Vector Int)
getShuffledRangeInts n = Immutable.Shuffle.shuffleM $ VB.enumFromTo 0 (n - 1)

type IsSimpleMap map k = (IsMap map, IsMap.Key map ~ k, IsMap.Value map ~ k)

benchInsert :: forall map k. IsSimpleMap map k => Proxy map -> VB.Vector k -> IO map
benchInsert _ ks = do
  m <- IsMap.new @map
  Foldable.foldlM (\m k -> IsMap.insert k k $! m) m ks

benchAll :: forall map k. IsSimpleMap map k => Proxy map -> VB.Vector k -> IO map
benchAll _ ks = do
  m <- IsMap.new @map
  m <- Foldable.foldlM (\m k -> IsMap.insert k k $! m) m ks
  m <- Foldable.foldlM (\m k -> IsMap.delete k m) m $ VB.take (VB.length ks `div` 2) ks
  VB.forM_ ks $ \k -> IsMap.lookup k m
  pure m

benchLookup :: forall map k. (IsMap map, IsMap.Key map ~ k, IsMap.Value map ~ k) => Proxy map -> VB.Vector k -> VB.Vector k -> IO map
benchLookup _ ks ks' = do
  m <- IsMap.new @map
  m <- Foldable.foldlM (\m k -> IsMap.insert k k $! m) m ks
  VB.forM_ ks' $ \k' -> IsMap.lookup k' m
  pure m
