{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Vector.Persistent.Internal.Strict
  ( snoc,
    adjust,
    update,
    (//),
    adjustF,
  )
where

import qualified Data.Foldable as Foldable
import Data.Vector.Persistent.Internal hiding (adjust, adjustF, snoc, update, (//))
import qualified Data.Vector.Persistent.Internal as Internal
import Prelude hiding (init, lookup)

-- | \( O(1) \) Append an element to the end of the vector.
snoc :: Vector a -> a -> Vector a
snoc vec !a = Internal.snoc vec a
{-# INLINE snoc #-}

adjust :: (a -> a) -> Int -> Vector a -> Vector a
adjust f = adjust# $ \x -> let !x' = f x in (# x' #)
{-# INLINE adjust #-}

adjustF :: Applicative f => (a -> f a) -> Int -> Vector a -> f (Vector a)
adjustF f ix vec = case lookup ix vec of
  Nothing -> pure vec
  Just x -> (\x' -> update ix x' vec) <$> f x
{-# INLINABLE adjustF #-}

update :: Int -> a -> Vector a -> Vector a
update i !a = Internal.update i a

{-# INLINE update #-}

(//) :: Vector a -> [(Int, a)] -> Vector a
(//) vec = Foldable.foldl' (flip $ uncurry update) vec
{-# INLINABLE (//) #-}
