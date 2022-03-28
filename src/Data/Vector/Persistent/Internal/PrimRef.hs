{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Vector.Persistent.Internal.PrimRef
  ( PrimRef,
    new,
    read,
    write,
    modify,
    modify'
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Primitive (Prim)
import Data.Primitive.PrimArray
import Prelude hiding (read)

newtype PrimRef s a = PrimRef (MutablePrimArray s a)
  deriving (Eq, NFData)

new :: (PrimMonad m, Prim a) => a -> m (PrimRef (PrimState m) a)
new x = do
  arr <- newPrimArray 1
  writePrimArray arr 0 x
  pure $ PrimRef arr

read :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> m a
read (PrimRef ref) = readPrimArray ref 0

write :: (PrimMonad m, Prim a) => a -> PrimRef (PrimState m) a -> m ()
write x (PrimRef ref) = writePrimArray ref 0 x

modify :: (PrimMonad m, Prim a) => (a -> a) -> PrimRef (PrimState m) a -> m ()
modify f ref = do
  x <- read ref
  write (f x) ref

modify' :: (PrimMonad m, Prim a) => (a -> a) -> PrimRef (PrimState m) a -> m ()
modify' f ref = do
  x <- read ref
  let !x' = f x
  write x' ref
