module IsMap where

import Data.HashMap.Mutable qualified as HashMap.Mutable.Arena

class IsMap map where
  type Key map
  type Value map
  new :: m map
  insert :: Key map -> Value map -> map -> m map
  delete :: Key map -> map -> m map
  lookup :: Key map -> map -> m (Maybe (Value map))

-- instance IsMap (HashMap.Mutable.Arena)
