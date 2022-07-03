{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad (void)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Random (MonadRandom)
import Control.Monad.Random.Strict qualified as Monad.Random
import Criterion.Main
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.HashMap.Mutable qualified as HashMap.Mutable.Arena
import Data.HashMap.Mutable.Internal.Robin qualified as HashMap.Mutable.Robin
import Data.Primitive (Array)
import Data.Vector qualified as VB
import Data.Vector.Hashtables qualified as Vector.Hashtables
import Data.Vector.Hashtables.Internal qualified as VH
import Data.Vector.Mutable qualified as VBM
import Immutable.Shuffle qualified
import MapFunctions (MapFunctions (..))
import MapFunctions qualified hiding (lookup)
import System.Random (Random)
import System.Random qualified as Random
import Prelude hiding (lookup)

insertMonotonic :: Int -> (Int -> IO h) -> (Int -> Int -> h -> IO ()) -> (Int -> h -> IO (Maybe a)) -> IO h
insertMonotonic n new insert lookup = do
  map <- new n
  let go1 i
        | i <= n = do insert i i map; go1 $ i + 1
        | otherwise = pure ()
  go1 0
  let go2 i
        | i <= n = do lookup i map; go2 $ i + 2
        | otherwise = pure ()
  go2 0
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
  let inputs = [100, 1000, 10000, 100000]
  defaultMain $ fmap makeBenches inputs

makeBenches :: Int -> Benchmark
makeBenches n =
  bgroup (show n) [makeIntBenches n]

makeBenches' :: Int -> Benchmark
makeBenches' n =
  bgroup
    (show n)
    [ bgroup
        "insert"
        [ bench "mutable-hashmaps arena" $
            whnfIO $
              insertMonotonic
                n
                (HashMap.Mutable.Arena.newWithCapacity)
                HashMap.Mutable.Arena.insert
                HashMap.Mutable.Arena.lookup,
          bench "mutable-hashmaps robin" $
            whnfIO $
              insertMonotonic
                n
                (HashMap.Mutable.Robin.newWithCapacity @Array)
                HashMap.Mutable.Robin.insert
                HashMap.Mutable.Robin.lookup,
          bench "vector-hashtables" $
            whnfIO $
              insertMonotonic
                n
                (Vector.Hashtables.initialize @VBM.MVector @Int @VBM.MVector @Int)
                (\i j m -> Vector.Hashtables.insert m i j)
                (\i m -> Vector.Hashtables.lookup m i)
        ]
        -- bgroup
        --   "insert resize"
        --   [ bench "mutable-hashmaps arena" $
        --       whnfIO $
        --         insertMonotonic
        --           n
        --           (const (HashMap.Mutable.Arena.new))
        --           HashMap.Mutable.Arena.insert,
        --     bench "mutable-hashmaps robin" $
        --       whnfIO $
        --         insertMonotonic
        --           n
        --           (const (HashMap.Mutable.Robin.new @Array))
        --           HashMap.Mutable.Robin.insert,
        --     bench "vector-hashtables" $
        --       whnfIO $
        --         insertMonotonic
        --           n
        --           (const $ Vector.Hashtables.initialize @VBM.MVector @Int @VBM.MVector @Int 10)
        --           (\i j m -> Vector.Hashtables.insert m i j),
        --     bench "hashtables basic" $
        --       whnfIO $
        --         insertMonotonic
        --           n
        --           (const (HashTable.new :: IO (HashTable.BasicHashTable Int Int)))
        --           (\i j m -> HashTable.insert m i j)
        --   ]
    ]

makeIntBenches :: Int -> Benchmark
makeIntBenches n =
  env
    ( do
        stdGen <- Random.newStdGen
        flip Monad.Random.evalRandT stdGen $ do
          fullInts <- getFullInts n (0, n - 1)
          fullIntsShuffled <- Immutable.Shuffle.shuffleM fullInts
          rangeInts <- getShuffledRangeInts (0, n - 1)
          pure (stdGen, fullInts, fullIntsShuffled, rangeInts)
    )
    withEnv
  where
    withEnv ~(stdGen, fullInts, fullIntsShuffled, rangeInts) =
      bgroup
        "int"
        [ lookupFullBenches,
          lookupMissBenches,
          insertFullBenches,
          insertRangeBenches
        ]
      where
        allBenches =
          bgroup "all" $
            MapFunctions.allMapFunctions
              <&> \fs -> bench (name fs) (whnfIO (benchAll fs fullInts))
        insertFullBenches =
          bgroup "insertFull" $
            MapFunctions.allMapFunctions
              <&> \fs -> bench (name fs) (whnfIO (benchInsert fs fullInts))
        insertRangeBenches =
          bgroup "insertRange" $
            MapFunctions.allMapFunctions
              <&> \fs -> bench (name fs) (whnfIO $ benchInsert fs rangeInts)
        lookupFullBenches =
          bgroup "lookupFull" $
            MapFunctions.allMapFunctions
              <&> \fs -> bench (name fs) (whnfIO $ benchLookup fs fullInts fullIntsShuffled)
        lookupMissBenches =
          env
            ( flip Monad.Random.evalRandT stdGen $ do
                is <- getFullInts n (0, maxBound)
                is' <- getFullInts n (minBound, -3)
                pure (is, is')
            )
            withEnv
          where
            withEnv ~(is, is') =
              bgroup "lookupMiss" $
                MapFunctions.allMapFunctions
                  <&> \fs -> bench (name fs) (whnfIO $ benchLookup fs is is')

-- benches =

getRandomVec :: (Random a, MonadRandom m) => m (VB.Vector a)
getRandomVec = VB.fromList <$> Monad.Random.getRandoms

getRandomVecRs :: (Random a, MonadRandom m) => (a, a) -> m (VB.Vector a)
getRandomVecRs rs = VB.fromList <$> Monad.Random.getRandomRs rs

getFullInts :: MonadRandom m => Int -> (Int, Int) -> m (VB.Vector Int)
getFullInts n r = VB.generateM n $ \_ -> Monad.Random.getRandomR r

getShuffledRangeInts :: (MonadRandom m, PrimMonad m) => (Int, Int) -> m (VB.Vector Int)
getShuffledRangeInts (i, j) = Immutable.Shuffle.shuffleM $ VB.enumFromTo i j

benchInsert :: MapFunctions k k -> VB.Vector k -> IO ()
benchInsert MapFunctions {new, insert} ks = do
  m <- new
  void $ evaluate $ Foldable.foldlM (\m k -> insert k k $! m) m ks

benchAll :: MapFunctions k k -> VB.Vector k -> IO ()
benchAll MapFunctions {new, insert, delete, lookup} ks = do
  m <- new
  m <- Foldable.foldlM (\m k -> insert k k $! m) m ks
  m <- Foldable.foldlM (\m k -> delete k m) m $ VB.take (VB.length ks `div` 2) ks
  VB.forM_ ks $ \k -> lookup k m
  void $ evaluate m

benchLookup :: MapFunctions k k -> VB.Vector k -> VB.Vector k -> IO ()
benchLookup MapFunctions {new, insert, lookup} ks ks' = do
  m <- new
  m <- Foldable.foldlM (\m k -> insert k k $! m) m ks
  VB.forM_ ks' $ \k' -> lookup k' m
  void $ evaluate m
