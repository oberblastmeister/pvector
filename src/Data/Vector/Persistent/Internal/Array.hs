module Data.Vector.Persistent.Internal.Array where

import Data.Primitive.SmallArray

type Array = SmallArray

type MArray = SmallMutableArray

singleton :: a -> Array a
singleton a = runSmallArray $ newSmallArray 1 a
{-# INLINE singleton #-}

empty :: Array a
empty = emptySmallArray
{-# INLINE empty #-}

update :: Array a -> Int -> a -> Array a
update arr i a = runSmallArray $ do
  marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
  writeSmallArray marr i a
  pure marr
{-# INLINE update #-}

index :: SmallArray a -> Int -> a
index = indexSmallArray
{-# INLINE index #-}

fromListN :: Int -> [a] -> SmallArray a
fromListN = smallArrayFromListN
{-# INLINE fromListN #-}

length :: SmallArray a -> Int
length = sizeofSmallArray
{-# INLINE length #-}
