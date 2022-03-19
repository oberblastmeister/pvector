{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Data.Foldable (foldl')
import Data.Primitive (SmallArray)
import qualified Data.Vector.Persistent.Internal.Array as Array

main :: IO ()
main =
  defaultMainWith
    defaultConfig
    [undefined]

-- copyBenches :: Benchmark
-- copyBenches =
--   bgroup
--     "copy"
--     [ copyBenchesWithArr (Array.run $ Array.new_ i)
--       | i <- arrSizes
--     ]
-- {-# NOINLINE copyBenches #-}

-- arrSizes :: [Int]
-- arrSizes = [1, 16, 32]

-- copyBenchesWithArr :: SmallArray Int -> Benchmark
-- copyBenchesWithArr !arr =
--   bgroup
--     ("size " ++ show (Array.length arr))
--     [ copyBench i arr
--       | i <- take 5 amounts
--     ]
-- {-# NOINLINE copyBenchesWithArr #-}

-- copyBench :: Int -> SmallArray Int -> Benchmark
-- copyBench amount !arr = bench (show amount) $ whnf (copyN amount) arr
-- {-# NOINLINE copyBench #-}

-- copyN :: Int -> SmallArray a -> SmallArray a
-- copyN i arr
--   | i < 0 = error "less than zero"
--   | otherwise = foldl' (\arr _i -> copy arr) arr [1 .. i]
-- {-# INLINE copyN #-}

-- copy :: SmallArray a -> SmallArray a
-- copy arr = Array.create_ len $ \marr -> do
--   Array.copy marr 0 arr 0 len
--   where
--     len = Array.length arr
-- {-# INLINE copy #-}

-- amounts :: [Int]
-- amounts = [10 ^ i | i <- [0 :: Int ..]]
