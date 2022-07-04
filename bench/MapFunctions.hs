module MapFunctions where

import Data.Functor (($>))
import Data.HashMap.Mutable.Internal.Generic qualified as Arena
import Data.HashMap.Strict qualified as Data.HashMap
import Data.Hashable (Hashable)
import Data.Primitive.Array (Array)
import Data.Primitive.PrimArray (PrimArray)
import Data.Vector.Hashtables qualified as Vector.Hashtables
import Data.Vector.Mutable qualified as VBM

data MapFunctions k v = forall map.
  MapFunctions
  { name :: String,
    new :: IO map,
    insert :: k -> v -> map -> IO map,
    delete :: k -> map -> IO map,
    lookup :: k -> map -> IO (Maybe v)
  }

arenaFunctions :: Hashable k => MapFunctions k v
arenaFunctions =
  MapFunctions
    { name = "mutable-hashmap arena",
      new = Arena.new @Array @Array,
      insert = \key value map -> Arena.insert key value map $> map,
      delete = \key map -> Arena.delete key map $> map,
      lookup = \key map -> Arena.lookup key map
    }
{-# SPECIALIZE arenaFunctions :: MapFunctions Int Int #-}

vectorFunctions :: forall k v. Hashable k => MapFunctions k v
vectorFunctions =
  MapFunctions
    { name = "vector-hashtables",
      new = Vector.Hashtables.initialize @VBM.MVector @k @VBM.MVector @v 10,
      insert = \key value map -> Vector.Hashtables.insert map key value $> map,
      lookup = \key map -> Vector.Hashtables.lookup map key,
      delete = \key map -> Vector.Hashtables.delete map key $> map
    }
{-# SPECIALIZE vectorFunctions :: MapFunctions Int Int #-}

unorderedContainersFunctions :: Hashable k => MapFunctions k v
unorderedContainersFunctions =
  MapFunctions
    { name = "unordered-containers",
      new = pure mempty,
      insert = \key value map -> pure $! Data.HashMap.insert key value map,
      lookup = \key map -> pure $! Data.HashMap.lookup key map,
      delete = \key map -> pure $! Data.HashMap.delete key map
    }
{-# SPECIALIZE unorderedContainersFunctions :: MapFunctions Int Int #-}

allMapFunctions :: Hashable k => [MapFunctions k v]
allMapFunctions =
  [ arenaFunctions,
    vectorFunctions,
    unorderedContainersFunctions
  ]
{-# SPECIALIZE allMapFunctions :: [MapFunctions Int Int] #-}

unboxedArenaFunctions :: MapFunctions Int Int
unboxedArenaFunctions =
  MapFunctions
    { name = "mutable-hashmap unboxed arena",
      new = Arena.new @PrimArray @PrimArray,
      insert = \key value map -> Arena.insert key value map $> map,
      delete = \key map -> Arena.delete key map $> map,
      lookup = \key map -> Arena.lookup key map
    }

unboxedIntMapFunctions :: [MapFunctions Int Int]
unboxedIntMapFunctions =
  [ unboxedArenaFunctions
  ]
