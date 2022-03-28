{-# LANGUAGE CPP #-}

module Data.Vector.Persistent.Internal.Bundle
  ( MBundle (..),
    Bundle,
    fromList,
    lift,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Vector.Fusion.Stream.Monadic (Stream (..))
import qualified Data.Vector.Fusion.Stream.Monadic as Stream

#include "vector.h"

data MBundle m a = MBundle
  { stream :: Stream m a,
    mSize :: Maybe Int
  }

type Bundle = MBundle Identity

-- | Convert a pure stream to a monadic stream
lift :: Monad m => MBundle Identity a -> MBundle m a
{-# INLINE_FUSED lift #-}
lift (MBundle {stream = (Stream step s), mSize}) =
  MBundle {stream = (Stream (return . runIdentity . step) s), mSize}

fromList :: [a] -> Bundle a
fromList xs = MBundle {stream = Stream.fromList xs, mSize = Nothing}
