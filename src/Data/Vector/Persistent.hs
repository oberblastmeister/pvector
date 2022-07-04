-- |
-- The @'Vector' a@ type is an persistent vector of elements of type @a@.
--
-- This module should be imported qualified, to avoid name clashes with the "Prelude".
-- 
-- Many operations have a average-case complexity of /O(log n)/.  The
-- implementation uses a large base (i.e. 32) so in practice these
-- operations are constant time.
--
-- = Comparison with Data.RRBVector and Data.Sequence
-- * Persistent vectors generally have less operations than sequences or RRBVectors but those operations can be faster.
-- * Persistent vectors have the fastest indexing.
-- * Persistent vectors are faster than RRBVectors at snocing because of tail optimization.
--   Snocing is a near constant time operation.
--   Snocing is still slower than sequences.
-- * RRBVectors are faster than persistent vectors at splitting and merging, but still slower than sequences.
-- * RRBVectors are faster than Sequences at indexing but slower than persistent vectors.
-- * Sequences have the fastest consing, snocing, and merging, but the slowest indexing.
-- 
-- ![diagram](docs/diagram.png)
module Data.Vector.Persistent
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
    adjustF,
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

import Data.Vector.Persistent.Internal
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
