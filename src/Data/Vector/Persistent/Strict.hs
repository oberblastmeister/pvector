module Data.Vector.Persistent.Strict
  ( foldr,
    foldr',
    foldl,
    foldl',
    Vector ((:|>), Empty),
    (|>),
    empty,
    length,
    lookup,
    index,
    (!?),
    (!),
    update,
    adjust,
    snoc,
    singleton,
    null,
    (//),
    (><),
    map,
    traverse,
    toList,
    fromList,
    unsnoc,
  )
where

import Data.Vector.Persistent.Internal hiding (adjust, snoc, update, (//))
import Data.Vector.Persistent.Strict.Internal
import Prelude hiding
  ( filter,
    foldl,
    foldr,
    length,
    lookup,
    map,
    null,
    reverse,
    traverse,
  )
