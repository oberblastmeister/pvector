{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Data.Vector.Persistent.Internal.Array where

import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Primitive.SmallArray
import Prelude hiding (length)

type Array = SmallArray

type MArray = SmallMutableArray

run = runSmallArray

new :: _ => _
new = newSmallArray
{-# INLINE new #-}

create = createSmallArray
{-# INLINE create #-}

write :: _ => _
write = writeSmallArray
{-# INLINE write #-}

read :: _ => _
read = readSmallArray
{-# INLINE read #-}

unsafeThaw :: _ => _
unsafeThaw = unsafeThawSmallArray
{-# INLINE unsafeThaw #-}

unsafeFreeze :: _ => _
unsafeFreeze = unsafeFreezeSmallArray
{-# INLINE unsafeFreeze #-}

shrink :: _ => _
shrink = shrinkSmallMutableArray
{-# INLINE shrink #-}

copy :: _ => _
copy = copySmallArray
{-# INLINE copy #-}

null :: SmallArray a -> Bool
null arr = length arr == 0
{-# INLINE null #-}

last :: SmallArray a -> a
last arr = index arr $ length arr

singleton :: a -> Array a
singleton a = runSmallArray $ newSmallArray 1 a
{-# INLINE singleton #-}

empty = emptySmallArray
{-# INLINE empty #-}

map :: (a -> b) -> SmallArray a -> SmallArray b
map = fmap
{-# INLINE map #-}

map' :: (a -> b) -> SmallArray a -> SmallArray b
map' = mapSmallArray'
{-# INLINE map' #-}

update :: Array a -> Int -> a -> Array a
update arr i a = runSmallArray $ do
  marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
  writeSmallArray marr i a
  pure marr
{-# INLINE update #-}

modify :: Array a -> Int -> (a -> a) -> Array a
modify arr i f = update arr i $ f $! index arr i
{-# INLINE modify #-}

modify' :: Array a -> Int -> (a -> a) -> Array a
modify' arr i f = update arr i $! f $! index arr i
{-# INLINE modify' #-}

updateResize :: Array a -> Int -> a -> Array a
updateResize arr i a = createSmallArray (max len (i + 1)) undefinedElem $ \marr -> do
  copySmallArray marr 0 arr 0 len
  writeSmallArray marr i a
  where
    len = sizeofSmallArray arr
{-# INLINE updateResize #-}

pop :: Array a -> Array a
pop arr = runSmallArray $ thawSmallArray arr 0 (length arr - 1)
{-# INLINE pop #-}

index = indexSmallArray
{-# INLINE index #-}

index# = indexSmallArray##
{-# INLINE index# #-}

fromListN = smallArrayFromListN
{-# INLINE fromListN #-}

length = sizeofSmallArray
{-# INLINE length #-}

undefinedElem :: forall a. a
undefinedElem = error "undefined element"
{-# NOINLINE undefinedElem #-}

fromListChunk :: PrimMonad m => Int -> a -> [a] -> m (MArray (PrimState m) a, [a], Int)
fromListChunk size def xs = do
  marr <- newSmallArray size def
  let go xs i
        | i == size = pure (xs, i)
        | otherwise = case xs of
            x : xs -> do
              writeSmallArray marr i x
              go xs (i + 1)
            [] -> pure ([], i)
  (xs', i') <- go xs 0
  pure (marr, xs', i')
{-# INLINE fromListChunk #-}
