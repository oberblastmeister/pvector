module Data.Vector.Persistent
  ( Vector,
    empty,
    length,
    indexMaybe,
    index,
    (!?),
    (!),
    update,
    snoc,
    singleton,
    null,
  )
where

import Data.Vector.Persistent.Internal
import Prelude hiding (length, null)
