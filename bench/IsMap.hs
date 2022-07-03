module IsMap where

import Data.Functor (($>))
import Data.HashMap.Mutable qualified as Arena
import Data.HashMap.Mutable.Internal.Robin qualified as Robin
import Data.Hashable (Hashable)
import GHC.Exts (RealWorld)

data MapFunctions k v = forall map.
  MapFunctions
  { new :: IO map,
    insert :: k -> v -> map -> IO map,
    delete :: k -> map -> IO map,
    lookup :: k -> map -> IO (Maybe v)
  }

arenaFunctions :: Hashable k => MapFunctions k v
arenaFunctions =
  MapFunctions
    { new = Arena.new,
      insert = \key value map -> Arena.insert key value map $> map,
      delete = \key map -> Arena.delete key map $> map,
      lookup = \key map -> Arena.lookup key map
    }

robinFunctions :: Hashable k => MapFunctions k v
robinFunctions =
  MapFunctions
    { new = Robin.new,
      insert = \key value map -> Robin.insert key value map $> map,
      -- delete = \key value map -> Robin.delete key value map $> map,
      lookup = \key map -> Robin.lookup key map
    }
