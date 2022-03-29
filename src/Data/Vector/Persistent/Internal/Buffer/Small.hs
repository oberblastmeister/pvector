module Data.Vector.Persistent.Internal.Buffer.Small where

import Control.Monad.Primitive
import Data.Primitive.SmallArray
import Prelude hiding (length)

data Buffer s a = Buffer
  { offset :: !Int,
    marr :: !(SmallMutableArray s a)
  }

new :: (PrimMonad m, s ~ PrimState m) => m (Buffer s a)
new = do
  marr <- newSmallArray 0 undefinedElem
  pure Buffer {offset = 0, marr}
{-# INLINE new #-}

newWithCapacity :: (PrimMonad m, s ~ PrimState m) => Int -> m (Buffer s a)
newWithCapacity cap = do
  marr <- newSmallArray cap undefinedElem
  pure Buffer {offset = 0, marr}
{-# INLINE newWithCapacity #-}

push :: (PrimMonad m, s ~ PrimState m) => a -> Buffer s a -> m (Buffer s a)
push a buffer = do
  buffer' <-
    if length buffer == capacity buffer
      then resize buffer
      else pure buffer
  writeSmallArray (marr buffer') (length buffer) a
  pure buffer' {offset = offset buffer' + 1}
{-# INLINE push #-}

read :: (PrimMonad m, s ~ PrimState m) => Int -> Buffer s a -> m a
read i Buffer {marr} = readSmallArray marr i
{-# INLINE read #-}

write :: (PrimMonad m, s ~ PrimState m) => Int -> a -> Buffer s a -> m ()
write i a Buffer {marr} = writeSmallArray marr i a
{-# INLINE write #-}

clear :: Buffer s a -> Buffer s a
clear = shrink 0
{-# INLINE clear #-}

shrink :: Int -> Buffer s a -> Buffer s a
shrink i buffer = buffer {offset = i}
{-# INLINE shrink #-}

unsafeShrink :: (PrimMonad m, s ~ PrimState m) => Int -> Buffer s a -> m (Buffer s a)
unsafeShrink i Buffer {marr} = do
  shrinkSmallMutableArray marr i
  pure Buffer {marr, offset = i}
{-# INLINE unsafeShrink #-}

capacity :: Buffer s a -> Int
capacity Buffer {marr} = sizeofSmallMutableArray marr
{-# INLINE capacity #-}

null :: Buffer s a -> Bool
null = (0 ==) . length

length :: Buffer s a -> Int
length = offset
{-# INLINE length #-}

undefinedElem :: forall a. a
undefinedElem = error "undefined element"
{-# NOINLINE undefinedElem #-}

resize :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m (Buffer s a)
resize buffer = do
  if capacity buffer == 0
    then grow 32 buffer
    else grow (capacity buffer) buffer
{-# INLINE resize #-}

grow :: (PrimMonad m, s ~ PrimState m) => Int -> Buffer s a -> m (Buffer s a)
grow more buffer@Buffer {marr, offset} = do
  marr' <- newSmallArray (sizeofSmallMutableArray marr + more) undefinedElem
  copySmallMutableArray marr' 0 marr 0 offset
  pure buffer {marr = marr'}
{-# INLINE grow #-}

freeze :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m (SmallArray a)
freeze Buffer {marr, offset} = freezeSmallArray marr 0 offset
{-# INLINE freeze #-}

unsafeFreeze :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m (SmallArray a)
unsafeFreeze Buffer {marr, offset} = do
  shrinkSmallMutableArray marr offset
  unsafeFreezeSmallArray marr
{-# INLINE unsafeFreeze #-}
