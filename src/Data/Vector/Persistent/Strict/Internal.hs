module Data.Vector.Persistent.Strict.Internal
  ( snoc,
    adjust,
    update,
    (//),
  )
where

import Data.Bits ((.&.))
import qualified Data.Foldable as Foldable
import Data.Vector.Persistent.Internal hiding (adjust, snoc, update, (//))
import qualified Data.Vector.Persistent.Internal.Array as Array
import Prelude hiding (init)

-- | \( O(1) \) Append an element to the end of the vector.
snoc :: Vector a -> a -> Vector a
snoc vec@RootNode {size, tail} !a
  -- Room in tail, and vector non-empty
  | (size .&. keyMask) /= 0 =
      vec
        { tail = Array.updateResize tail (size .&. keyMask) a,
          size = size + 1
        }
  | otherwise = snocArr vec 1 $ Array.singleton a
{-# INLINE snoc #-}

adjust :: (a -> a) -> Int -> Vector a -> Vector a
adjust f ix vec@RootNode {size, shift, tail}
  -- Invalid index. This funny business uses a single test to determine whether
  -- ix is too small (negative) or too large (at least sz).
  | (fromIntegral ix :: Word) >= fromIntegral size = vec
  | ix >= tailOffset vec = vec {tail = Array.modify' tail (ix .&. keyMask) f}
  | otherwise = vec {init = go shift (init vec)}
  where
    go level vec
      | level == keyBits,
        let !node = DataNode $ Array.modify' (getDataNode vec') (ix .&. keyMask) f =
          Array.update vec ix' node
      | otherwise,
        let !node = go (level - keyBits) (getInternalNode vec') =
          Array.update vec ix' $! InternalNode node
      where
        ix' = (ix !>>. level) .&. keyBits
        vec' = Array.index vec ix'
{-# INLINE adjust #-}

update :: Int -> a -> Vector a -> Vector a
update ix !a vec@RootNode {size, shift, tail}
  -- Invalid index. This funny business uses a single test to determine whether
  -- ix is too small (negative) or too large (at least sz).
  | (fromIntegral ix :: Word) >= fromIntegral size = vec
  | ix >= tailOffset vec = vec {tail = Array.update tail (ix .&. keyMask) a}
  | otherwise = vec {init = go shift (init vec)}
  where
    go level vec
      | level == keyBits,
        let !node = DataNode $ Array.update (getDataNode vec') (ix .&. keyMask) a =
          Array.update vec ix' node
      | otherwise,
        let !node = go (level - keyBits) (getInternalNode vec') =
          Array.update vec ix' $! InternalNode node
      where
        ix' = (ix !>>. level) .&. keyMask
        vec' = Array.index vec ix'
{-# INLINE update #-}

-- Note: we fully apply foldl' to get everything to unbox.
(//) :: Vector a -> [(Int, a)] -> Vector a
(//) vec = Foldable.foldl' go vec
  where
    go v (ix, !a) = update ix a v
