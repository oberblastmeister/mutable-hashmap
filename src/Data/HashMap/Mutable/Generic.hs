module Data.HashMap.Mutable.Generic
  ( HashMap,
    HasArray,
    BothElement,
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

import Data.HashMap.Mutable.Internal.Generic
import Prelude hiding (lookup)
