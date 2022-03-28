module Data.Vector.Persistent.Internal.Buffer.Large where

import Control.Monad.Primitive
import Data.Primitive.Array
import Data.Primitive.SmallArray
import Debug.Trace (trace)
import Prelude hiding (length)

data Buffer s a = Buffer
  { offset :: !Int,
    marr :: !(MutableArray s a)
  }

new :: (PrimMonad m, s ~ PrimState m) => m (Buffer s a)
new = newWithCapacity 0
{-# INLINE new #-}

newWithCapacity :: (PrimMonad m, s ~ PrimState m) => Int -> m (Buffer s a)
newWithCapacity cap = do
  marr <- newArray cap undefinedElem
  pure Buffer {offset = 0, marr}
{-# INLINE newWithCapacity #-}

push :: (PrimMonad m, s ~ PrimState m) => a -> Buffer s a -> m (Buffer s a)
push a buffer = do
  buffer' <-
    if length buffer == capacity buffer
      then resize buffer
      else pure buffer
  writeArray (marr buffer') (length buffer) a
  pure buffer' {offset = offset buffer' + 1}
{-# INLINE push #-}

read :: (PrimMonad m, s ~ PrimState m) => Int -> Buffer s a -> m a
read i Buffer {marr} = readArray marr i
{-# INLINE read #-}

write :: (PrimMonad m, s ~ PrimState m) => Int -> a -> Buffer s a -> m ()
write i a Buffer {marr} = writeArray marr i a
{-# INLINE write #-}

clear :: Buffer s a -> Buffer s a
clear = shrink 0
{-# INLINE clear #-}

shrink :: Int -> Buffer s a -> Buffer s a
shrink i buffer = buffer {offset = i}
{-# INLINE shrink #-}

capacity :: Buffer s a -> Int
capacity Buffer {marr} = sizeofMutableArray marr
{-# INLINE capacity #-}

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
grow more buffer@Buffer {offset, marr} = do
  marr' <- newArray (sizeofMutableArray marr + more) undefinedElem
  copyMutableArray marr' 0 marr 0 offset
  pure buffer {marr = marr'}
{-# INLINE grow #-}

empty :: Array a
empty = mempty

singleton :: a -> Array a
singleton = pure

freeze :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m (Array a)
freeze Buffer {marr, offset} = freezeArray marr 0 offset
{-# INLINE freeze #-}

toSmall :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m (SmallMutableArray s a)
toSmall Buffer {marr, offset} = do
  smallMarr <- newSmallArray offset undefinedElem
  let go i
        | i == offset = pure ()
        | otherwise = do
            x <- readArray marr i
            writeSmallArray smallMarr i x
            go $ i + 1
  go 0
  pure smallMarr

freezeSmall :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m (SmallArray a)
freezeSmall buffer = toSmall buffer >>= unsafeFreezeSmallArray
