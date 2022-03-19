{-# LANGUAGE RankNTypes #-}

module Data.Vector.Persistent.Internal.Array where

import Control.Monad.Primitive (PrimMonad (PrimState))
import Control.Monad.ST (ST)
import Data.Primitive.SmallArray
import Prelude hiding (length)

type Array = SmallArray

type MArray = SmallMutableArray

new :: PrimMonad m => Int -> a -> m (MArray (PrimState m) a)
new = newSmallArray
{-# INLINE new #-}

new_ :: PrimMonad m => Int -> m (MArray (PrimState m) a)
new_ n = new n undefinedElem
{-# INLINE new_ #-}

run :: (forall s. ST s (SmallMutableArray s a)) -> SmallArray a
run = runSmallArray

create :: Int -> a -> (forall s. SmallMutableArray s a -> ST s ()) -> SmallArray a
create = createSmallArray
{-# INLINE create #-}

create_ :: Int -> (forall s. SmallMutableArray s a -> ST s ()) -> SmallArray a
create_ i m = create i (error "impossible") m
{-# INLINE create_ #-}

copy ::
  PrimMonad m =>
  ( SmallMutableArray (PrimState m) a ->
    Int ->
    SmallArray a ->
    Int ->
    Int ->
    m ()
  )
copy = copySmallArray
{-# INLINE copy #-}

undefinedElem :: a
undefinedElem = error "Data.Vector.Persistent.Internal.Array: Undefined element"
{-# NOINLINE undefinedElem #-}

index :: Array a -> Int -> a
index = indexSmallArray
{-# INLINE index #-}

update :: Array a -> Int -> a -> Array a
update arr i a = run $ do
  marr <- thaw arr
  writeSmallArray marr i a
  pure marr
{-# INLINE update #-}

thaw :: PrimMonad m => Array a -> m (MArray (PrimState m) a)
thaw arr = thawSmallArray arr 0 (sizeofSmallArray arr)
{-# INLINE thaw #-}

empty :: SmallArray a
empty = emptySmallArray
{-# INLINE empty #-}

singleton :: a -> Array a
singleton a = run $ new 1 a
{-# INLINE singleton #-}

fromListN :: Int -> [a] -> SmallArray a
fromListN = smallArrayFromListN
{-# INLINE fromListN #-}

snoc :: Array a -> a -> Array a
snoc arr a = create_ len $ \marr -> do
  copySmallArray marr 0 arr 0 len
  writeSmallArray marr len a
  where
    len = length arr
{-# INLINE snoc #-}

length :: SmallArray a -> Int
length = sizeofSmallArray
{-# INLINE length #-}
