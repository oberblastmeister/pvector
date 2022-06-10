-- |
--
-- The implementation is based on /array mapped tries/.  A
-- 'Vector' is often faster than other tree-based vector types.
--
-- Many operations have a average-case complexity of \(O(\log n)\).  The
-- implementation uses a large base (i.e. 32) so in practice these
-- operations are constant time.
module Data.Vector.Persistent.Strict
  ( Vector ((:|>), Empty),

    -- * Construction
    empty,
    singleton,

    -- * Basic interface
    length,
    null,
    (!?),
    (!),
    unsnoc,
    snoc,
    lookup,
    index,
    update,
    adjust,
    (//),
    (|>),
    (><),

    -- * Folds
    foldr,
    foldr',
    foldl,
    foldl',

    -- * Transformations
    map,
    traverse,

    -- * Lists
    toList,
    fromList,
  )
where

import Data.Vector.Persistent.Internal hiding (adjust, snoc, update, (//))
import Data.Vector.Persistent.Internal.Strict
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
