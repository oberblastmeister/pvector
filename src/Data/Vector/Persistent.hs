{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Vector.Persistent where

import GHC.Exts

data Vector a = Vector
  { size :: Int#,
    level :: Int#,
    init :: AArray,
    tail :: Array
  }

data Node a
  = InternalNode ()
