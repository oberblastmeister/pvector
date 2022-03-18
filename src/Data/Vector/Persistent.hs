module Data.Vector.Persistent
  ( Vector,
    empty,
    length,
    indexMaybe,
    index,
  )
where

import Data.Vector.Persistent.Internal
import GHC.Stack (HasCallStack)
import Prelude hiding (length)

indexMaybe :: Vector a -> Int -> Maybe a
indexMaybe vec ix
  -- Check if the index is valid. This funny business uses a single test to
  -- determine whether ix is too small (negative) or too large (at least the
  -- length of the vector).
  | (fromIntegral ix :: Word) < fromIntegral (length vec) =
      Just $ unsafeIndex vec ix
  | otherwise = Nothing
{-# INLINE indexMaybe #-}

index :: Vector a -> Int -> a
index vec ix
  | ix < 0 = moduleError "index" $ "negative index: " ++ show ix
  | ix >= length vec = moduleError "index" $ "index too large: " ++ show ix
  | otherwise = unsafeIndex vec ix
{-# INLINE index #-}

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error ("Data.Vector.Persistent." ++ fun ++ ':' : ' ' : msg)
{-# NOINLINE moduleError #-}
