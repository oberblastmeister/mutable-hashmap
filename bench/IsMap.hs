module IsMap where

import Data.Functor (($>))
import Data.HashMap.Mutable qualified as HashMap.Mutable.Arena
import Data.Hashable (Hashable)
import GHC.Exts (RealWorld)

class IsMap map where
  type Key map
  type Value map
  new :: IO map
  insert :: Key map -> Value map -> map -> IO map
  delete :: Key map -> map -> IO map
  lookup :: Key map -> map -> IO (Maybe (Value map))

instance (Hashable k) => IsMap (HashMap.Mutable.Arena.HashMap RealWorld k v) where
  type Key (HashMap.Mutable.Arena.HashMap RealWorld k v) = k
  type Value (HashMap.Mutable.Arena.HashMap RealWorld k v) = v
  new = HashMap.Mutable.Arena.new
  insert key value map = HashMap.Mutable.Arena.insert key value map $> map
  delete key map = HashMap.Mutable.Arena.delete key map $> map
  lookup key map = HashMap.Mutable.Arena.lookup key map
