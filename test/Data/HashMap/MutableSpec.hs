{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.HashMap.MutableSpec (spec) where

import Control.Exception qualified as Exception
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimState)
import Data.Foldable (foldl')
import Data.HashMap.Mutable.Generic qualified as HashMap.Mutable
import Data.HashMap.Strict qualified as HashMap.Strict
import Data.List qualified as List
import Data.Primitive (Array)
import Data.Vector.Hashtables qualified as Vector.Hashtables
import Data.Vector.Mutable qualified as VBM
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "properties" $ do
    prop "lookup" propLookup

    prop "delete" propDelete

    prop "insert and delete" propInsertDelete

  it "should work" $ do
    map <- Vector.Hashtables.fromList @VBM.MVector @Int @VBM.MVector @Int [(1, 1), (2, 2), (3, 3)]
    Vector.Hashtables.delete map 2
    kvs <- Vector.Hashtables.toList map
    kvs `shouldBe` [(1, 1), (3, 3)]

  pure ()

propLookup :: [(Int, Int)] -> [Int] -> IO ()
propLookup kvs ks = do
  compareHashMaps kvs $ \map mutableMap -> do
    forM_ ks $ \k -> do
      v <- HashMap.Mutable.lookup k mutableMap
      let v' = HashMap.Strict.lookup k map
      v `shouldBe` v'
      pure ()
    pure map

propDelete :: [(Int, Int)] -> [Int] -> IO ()
propDelete kvs ks = do
  let dks = map fst (take 20 kvs) ++ ks
  compareHashMaps kvs $ \map mutableMap -> do
    let map' = foldl' (flip HashMap.Strict.delete) map dks
    forM_ dks $ \k -> HashMap.Mutable.delete k mutableMap
    pure map'

propInsertDelete :: [(Int, Int)] -> [Int] -> [(Int, Int)] -> IO ()
propInsertDelete kvs ks ikvs = do
  let dks = map fst (take 20 kvs) ++ ks
  compareHashMaps kvs $ \map mutableMap -> do
    let map' = HashMap.Strict.fromList ikvs `HashMap.Strict.union` foldl' (flip HashMap.Strict.delete) map dks
    forM_ dks $ \k -> HashMap.Mutable.delete k mutableMap
    forM_ ikvs $ \(k, v) -> HashMap.Mutable.insert k v mutableMap
    pure map'

compareHashMaps ::
  [(Int, Int)] ->
  ( HashMap.Strict.HashMap Int Int ->
    HashMap.Mutable.HashMap Array Array (PrimState IO) Int Int ->
    IO (HashMap.Strict.HashMap Int Int)
  ) ->
  IO ()
compareHashMaps kvs f = do
  let map = HashMap.Strict.fromList kvs
  mutableMap <- HashMap.Mutable.fromList kvs
  map' <- f map mutableMap
  let list1 = List.sort $ HashMap.Strict.toList map'
  list2 <- List.sort <$> HashMap.Mutable.toList mutableMap
  list1 `shouldBe` list2
  forM_ list1 $ \(k, _) -> do
    x <- HashMap.Mutable.lookup k mutableMap
    case x of
      Nothing -> Exception.throwIO $ Exception.ErrorCall "Not found"
      Just _ -> pure ()
