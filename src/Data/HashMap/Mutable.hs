module Data.HashMap.Mutable
  ( HashMap,
    insert,
    insert',
    delete,
    delete',
    lookup,
    lookup',
    new,
    newWithCapacity,
    toList,
    fromList,
  )
where

import Data.HashMap.Mutable.Internal.Arena
import Prelude hiding (lookup)
