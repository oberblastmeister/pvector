module Data.Vector.Persistent.Internal.Buffer.Mutable
  ( Buffer,
    new,
    newWithCapacity,
    push,
    read,
    write,
    length,
    capacity,
    null,
    clear,
    freeze,
    shrink,
  )
where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Primitive.Array
import Data.Primitive.MutVar
import Data.Vector.Persistent.Internal.PrimRef (PrimRef)
import qualified Data.Vector.Persistent.Internal.PrimRef as PrimRef
import Prelude hiding (length, null, read)

data Buffer s a = Buffer
  { offsetRef :: PrimRef s Int,
    marrRef :: MutVar s (MutableArray s a)
  }

new :: (PrimMonad m, s ~ PrimState m) => m (Buffer s a)
new = do
  offsetRef <- PrimRef.new 0
  marrRef <- newArray 0 undefinedElem >>= newMutVar
  pure Buffer {offsetRef, marrRef}
{-# INLINE new #-}

newWithCapacity :: (PrimMonad m, s ~ PrimState m) => Int -> m (Buffer s a)
newWithCapacity cap = do
  offsetRef <- PrimRef.new 0
  marrRef <- newArray cap undefinedElem >>= newMutVar
  pure Buffer {offsetRef, marrRef}
{-# INLINE newWithCapacity #-}

push :: (PrimMonad m, s ~ PrimState m) => a -> Buffer s a -> m ()
push a buffer = do
  len <- length buffer
  cap <- capacity buffer
  when (len == cap) $ resize buffer
  marrRef <- readMutVar $ marrRef buffer
  writeArray marrRef len a
  PrimRef.modify' (+ 1) (offsetRef buffer)
{-# INLINE push #-}

clear :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m ()
clear Buffer {offsetRef} = PrimRef.write 0 offsetRef
{-# INLINE clear #-}

shrink :: (PrimMonad m, s ~ PrimState m) => Int -> Buffer s a -> m ()
shrink offsetRef' Buffer {offsetRef} = PrimRef.write offsetRef' offsetRef
{-# INLINE shrink #-}

read :: (PrimMonad m, s ~ PrimState m) => Int -> Buffer s a -> m a
read i buffer = do
  marrRef <- readMutVar $ marrRef buffer
  readArray marrRef i
{-# INLINE read #-}

write :: (PrimMonad m, s ~ PrimState m) => Int -> a -> Buffer s a -> m ()
write i a buffer = do
  marrRef <- readMutVar $ marrRef buffer
  writeArray marrRef i a
{-# INLINE write #-}

resize :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m ()
resize buffer = do
  cap <- capacity buffer
  if cap == 0
    then grow 4 buffer
    else grow cap buffer
{-# INLINABLE resize #-}

grow :: (PrimMonad m, s ~ PrimState m) => Int -> Buffer s a -> m ()
grow more Buffer {marrRef} = do
  marr <- readMutVar marrRef
  marr' <- newArray (sizeofMutableArray marr + more) undefinedElem
  copyMutableArray marr' 0 marr 0 (sizeofMutableArray marr)
  writeMutVar marrRef marr'
{-# INLINABLE grow #-}

capacity :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m Int
capacity Buffer {marrRef} = sizeofMutableArray <$> readMutVar marrRef
{-# INLINE capacity #-}

null :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m Bool
null = fmap (== 0) . length
{-# INLINE null #-}

length :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m Int
length Buffer {offsetRef} = PrimRef.read offsetRef
{-# INLINE length #-}

undefinedElem :: forall a. a
undefinedElem = error "undefined element"
{-# NOINLINE undefinedElem #-}

freeze :: (PrimMonad m, s ~ PrimState m) => Buffer s a -> m (Array a)
freeze buffer@Buffer {marrRef} = do
  len <- length buffer
  marr <- readMutVar marrRef
  freezeArray marr 0 len
{-# INLINE freeze #-}
