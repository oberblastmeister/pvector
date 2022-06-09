module Data.Vector.Persistent.Internal.CoercibleUtils where

import Data.Coerce (Coercible, coerce)

-- | Coercive left-composition.
--
-- >>> (All #. not) True
-- All {getAll = False}
--
-- The semantics with respect to bottoms are:
--
-- @
-- p '#.' ⊥ ≡ ⊥
-- p '#.' f ≡ p '.' f
-- @
infixr 9 #.

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
{-# INLINE (#.) #-}

-- | Coercive right-composition.
--
-- >>> (stimes 2 .# Product) 3
-- Product {getProduct = 9}
--
-- The semantics with respect to bottoms are:
--
-- @
-- ⊥ '.#' p ≡ ⊥
-- f '.#' p ≡ p '.' f
-- @
infixr 9 .#

(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
{-# INLINE (.#) #-}
