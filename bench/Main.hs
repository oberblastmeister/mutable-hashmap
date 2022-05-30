module Main where

import Control.Monad.Primitive (PrimState)
import Criterion.Main
import Data.HashMap.Mutable.Internal.Generic qualified as HashMap.Mutable.Arena
import Data.HashTable.IO qualified as HashTable
import Data.Primitive (Array)
import Data.Vector.Hashtables qualified as Vector.Hashtables
import Data.Vector.Hashtables.Internal qualified as VH
import Data.Vector.Mutable qualified as VBM

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
