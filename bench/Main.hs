{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.DeepSeq (NFData)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST, runST)
import Criterion.Main
import Data.Foldable (foldl')
import Data.Functor.Identity (runIdentity)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.RRBVector as RRBVector
import qualified Data.Sequence as Seq
import qualified Data.Vector as VB
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import qualified Data.Vector.Persistent as Vector.Persistent
import qualified Data.Vector.Persistent.Internal as Vector.Persistent.Internal
import qualified Data.Vector.Persistent.Internal.Array as Vector.Persistent.Internal.Array
import qualified Data.Vector.Persistent.Internal.Buffer.Large as Buffer.Large
import qualified Data.Vector.Persistent.Internal.Buffer.Mutable as Buffer.Mutable
import qualified Data.Vector.Persistent.Internal.Bundle as Bundle
import GHC.Exts (IsList (..))

data Snocer where
  Snocer :: NFData (f Int) => String -> ([Int] -> f Int) -> (f Int -> Int -> f Int) -> Snocer

data FromList where
  FromList :: String -> ([Int] -> f Int) -> FromList

data Convert where
  Convert :: NFData (f Int) => String -> ([Int] -> f Int) -> (f Int -> f Int) -> Convert

data Indexer where
  Indexer :: NFData (f Int) => String -> ([Int] -> f Int) -> (f Int -> Int -> Int) -> Indexer

data VectorFolder where
  VectorFolder :: String -> (Vector.Persistent.Vector Int -> Int) -> VectorFolder

data BufferPusher where
  BufferPusher :: String -> (forall s. ST s (buf s Int)) -> (forall s. Int -> Int -> buf s Int -> ST s ()) -> BufferPusher

sampleHashMap :: [Int] -> HashMap Int Int
sampleHashMap is = HashMap.fromList $ fmap (\i -> (i, i)) is

snocHashMap :: HashMap Int Int -> Int -> HashMap Int Int
snocHashMap map i = HashMap.insert i i map

convertVectorStream :: Vector.Persistent.Vector a -> Vector.Persistent.Vector a
convertVectorStream vec =
  Vector.Persistent.Internal.unstream
    Bundle.MBundle
      { Bundle.stream = Vector.Persistent.Internal.stream vec,
        Bundle.mSize = Nothing
      }
{-# INLINE convertVectorStream #-}

convertVectorList :: Vector.Persistent.Vector a -> Vector.Persistent.Vector a
convertVectorList = Vector.Persistent.fromList . Vector.Persistent.toList
{-# INLINE convertVectorList #-}

convertVectorVector :: VB.Vector a -> VB.Vector a
convertVectorVector = fromList . toList
{-# NOINLINE convertVectorVector #-}

main :: IO ()
main =
  defaultMainWith
    defaultConfig
    [ bgroup "snoc" $
        snocs
          [ Snocer "Data.Vector.Persistent" fromList Vector.Persistent.snoc,
            Snocer "Data.RRBVector" fromList (RRBVector.|>),
            Snocer "Data.Vector" fromList VB.snoc,
            Snocer "Data.HashMap.Strict" sampleHashMap snocHashMap,
            Snocer "Data.Sequence" fromList (Seq.|>)
          ],
      bgroup "fromList" $
        fromLists
          [ FromList "Data.Vector.Persistent iterate" Vector.Persistent.Internal.fromListIterate,
            FromList "Data.Vector.Persistent stream" Vector.Persistent.Internal.fromListStream,
            FromList "Data.Vector.Persistent naive" (foldl' Vector.Persistent.snoc Vector.Persistent.empty),
            FromList "Data.RRBVector" RRBVector.fromList,
            FromList "Data.Vector" VB.fromList,
            FromList "Data.HashMap.Strict" sampleHashMap,
            FromList "Data.Sequence" Seq.fromList
          ],
      bgroup "convert" $
        converts
          [ Convert "convertVectorStream" fromList convertVectorStream,
            Convert "convertVectorList" fromList convertVectorList,
            Convert "convertVectorVector" fromList convertVectorVector
          ],
      bgroup "index" $
        indexers
          [ Indexer "Data.Vector.Persistent" fromList Vector.Persistent.index,
            Indexer "Data.RRBVector" fromList (flip RRBVector.index),
            Indexer "Data.Vector" fromList (VB.!),
            Indexer "Data.HashMap.Strict" sampleHashMap (HashMap.!),
            Indexer "Data.Sequence" fromList Seq.index
          ],
      bgroup "fold" $
        vectorFolders
          [ VectorFolder "normal" sum,
            VectorFolder "stream" (runIdentity . Stream.foldl' (+) 0 . Vector.Persistent.Internal.stream),
            VectorFolder "stream reversed" (runIdentity . Stream.foldl' (+) 0 . Vector.Persistent.Internal.streamR)
          ],
      bgroup "create arrays" $ createArrays,
      bgroup "push buffers" $
        pushBuffers
          [ BufferPusher "mut var" Buffer.Large.new pushBufferLarge,
            BufferPusher "not mut var" Buffer.Mutable.new pushBufferOther
          ]
    ]
  where
    bench' sections = bench $ List.intercalate "/" sections

    -- 1.5x faster than RRBVector
    -- 2x faster than HashMap
    -- way slower than Seq
    snocs funcs =
      [ env (pure $ sample [1 .. size]) $ \seq -> env (pure [1 .. amount]) $ \list ->
          ( bench'
              [title, "size " ++ show size, "amount " ++ show amount]
              (whnf (foldl' func seq) list)
          )
        | size <- sizes,
          amount <- amounts,
          Snocer title sample func <- funcs
      ]

    fromLists funcs =
      [ env (pure [1 .. size]) $ \list ->
          (bench' [title, "size " ++ show size] (whnf func list))
        | size <- sizes,
          FromList title func <- funcs
      ]

    converts funcs =
      [ env (pure $ fromList [1 .. size]) $ \vec ->
          (bench' [title, "size " ++ show size] (whnf func vec))
        | size <- sizes,
          Convert title fromList func <- funcs
      ]
    -- 1.5x faster than RRBVector
    -- 2x slower than Vector
    -- 2x faster than HashMap
    -- Seq has really slow indexing
    indexers funcs =
      [ env
          ( let seq = sample indices
                indices = [0 .. size - 1]
             in pure (seq, indices)
          )
          $ \(~(seq, indices)) ->
            ( bench'
                [title, "size " ++ show size]
                ( whnf
                    ( \indices ->
                        foldl'
                          (\seq index -> let !_ = func seq index in seq)
                          seq
                          indices
                    )
                    indices
                )
            )
        | size <- sizes,
          Indexer title sample func <- funcs
      ]

    vectorFolders funcs =
      [ env (pure $ Vector.Persistent.fromList [1 .. size]) $ \vec ->
          ( bench'
              [title, "size " ++ show size]
              (whnf func vec)
          )
        | size <- sizes,
          VectorFolder title func <- funcs
      ]

    createArrays =
      [ ( bench'
            ["size " ++ show size]
            ( whnf
                ( \size ->
                    Vector.Persistent.Internal.Array.run $
                      Vector.Persistent.Internal.Array.new
                        size
                        (1 :: Int)
                )
                size
            )
        )
        | size <- sizes
      ]

    pushBuffers funcs =
      [ ( bench'
            [title, "size " ++ show size]
            ( whnf
                ( \size -> runST $ do
                    buffer <- create
                    func 0 size buffer
                )
                size
            )
        )
        | size <- sizes,
          BufferPusher title create func <- funcs
      ]

pushBufferLarge :: (PrimMonad m, s ~ PrimState m) => a -> Int -> Buffer.Large.Buffer s a -> m ()
pushBufferLarge x times buffer
  | 0 <- times = pure ()
  | otherwise = do
      buffer' <- Buffer.Large.push x buffer
      pushBufferLarge x (times - 1) buffer'

pushBufferOther :: (PrimMonad m, s ~ PrimState m) => a -> Int -> Buffer.Mutable.Buffer s a -> m ()
pushBufferOther x times buffer
  | 0 <- times = pure ()
  | otherwise = do
      Buffer.Mutable.push x buffer
      pushBufferOther x (times - 1) buffer

-- snocBench :: String -> Int -> [a] -> ([a] -> seq) -> (seq -> a -> seq) -> Benchmark
-- snocBench name times input constructor snocer =
--   env (pure $ constructor input) $ \seq -> thisth
-- {-# NOINLINE snocBench #-}

-- onIntListByMagBenchList :: Int -> ([[Int] -> Benchmark]) -> Benchmark
-- onIntListByMagBenchList amount benchmarks =
--   onSizeByMagBenchList amount $ \size ->
--     env (pure $ enumFromTo 0 size) $ \input -> bgroup "bruh" (fmap ($ input) benchmarks)
-- benchmarks $! enumFromTo 0 size

-- onSizeByMagBenchList :: Int -> (Int -> Benchmark) -> [Benchmark]
-- onSizeByMagBenchList amount benchmarks =
--   [bgroup (show size) (benchmarks size) | size <- take amount sizesByMagnitude]
amounts :: [Int]
amounts = take 4 allSizes

sizes :: [Int]
sizes = take 4 allSizes

allSizes :: [Int]
allSizes = [10 ^ i | i <- [0 :: Int ..]]

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
