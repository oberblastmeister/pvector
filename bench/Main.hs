{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.DeepSeq (NFData)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST, runST)
import Criterion.Main
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.RRBVector as RRBVector
import qualified Data.Sequence as Seq
import qualified Data.Vector as VB
import qualified Data.Vector.Persistent as Vector.Persistent
import qualified Data.Vector.Persistent.Internal.Array as Vector.Persistent.Internal.Array
import qualified Data.Vector.Persistent.Internal.Buffer.Large as Buffer.Large
import qualified Data.Vector.Persistent.Internal.Buffer.Mutable as Buffer.Mutable
import GHC.Exts (IsList (..))

data Snocer where
  Snocer :: NFData (f Int) => String -> ([Int] -> f Int) -> (f Int -> Int -> f Int) -> Snocer

data FromList where
  FromList :: String -> ([Int] -> f Int) -> FromList

data Map where
  Map :: NFData (f Int) => String -> ([Int] -> f Int) -> (f Int -> f Int) -> Map

data Indexer where
  Indexer :: NFData (f Int) => String -> ([Int] -> f Int) -> (f Int -> Int -> Int) -> Indexer

data Updater where
  Updater :: NFData (f Int) => String -> ([Int] -> f Int) -> (f Int -> Int -> f Int) -> Updater

data Folder where
  Folder :: NFData (f Int) => String -> ([Int] -> f Int) -> (f Int -> Int) -> Folder

data BufferPusher where
  BufferPusher :: String -> (forall s. ST s (buf s Int)) -> (forall s. Int -> Int -> buf s Int -> ST s ()) -> BufferPusher

sampleHashMap :: [Int] -> HashMap Int Int
sampleHashMap is = HashMap.fromList $ fmap (\i -> (i, i)) is

snocHashMap :: HashMap Int Int -> Int -> HashMap Int Int
snocHashMap map i = HashMap.insert i i map

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
          [ FromList "Data.Vector.Persistent" Vector.Persistent.fromList,
            FromList "Data.Vector.Persistent naive" (foldl' Vector.Persistent.snoc Vector.Persistent.empty),
            FromList "Data.RRBVector" RRBVector.fromList,
            FromList "Data.Vector" VB.fromList,
            FromList "Data.HashMap.Strict" sampleHashMap,
            FromList "Data.Sequence" Seq.fromList
          ],
      bgroup "map" $
        maps
          [ Map "Data.Vector.Persistent" fromList (Vector.Persistent.map (20 +)),
            Map "Data.RRBVector" fromList (RRBVector.map (20 +))
          ],
      bgroup "index" $
        indexers
          [ Indexer "Data.Vector.Persistent" fromList Vector.Persistent.index,
            Indexer "Data.RRBVector" fromList (flip RRBVector.index),
            Indexer "Data.Vector" fromList (VB.!),
            Indexer "Data.HashMap.Strict" sampleHashMap (HashMap.!),
            Indexer "Data.Sequence" fromList Seq.index
          ],
      bgroup "update" $
        updaters
          [ Updater "Data.Vector.Persistent" fromList (\vec i -> Vector.Persistent.update vec i i),
            Updater "Data.RRBVector" fromList (\vec i -> RRBVector.update i i vec)
          ],
      bgroup "fold" $
        vectorFolders
          [ Folder "normal" Vector.Persistent.fromList sum,
            Folder "Data.RRBVector" Vector.Persistent.fromList sum
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

    -- 1.5x-2x faster than RRBVector
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
        | size <- sizesLarge,
          FromList title func <- funcs
      ]

    maps funcs =
      [ env (pure $ fromList [1 .. size]) $ \vec ->
          (bench' [title, "size " ++ show size] (whnf func vec))
        | size <- take 4 $ drop 4 allSizes,
          Map title fromList func <- funcs
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

    updaters funcs =
      [ env
          ( let seq = sample indices
                indices = [0 .. size - 1]
             in pure (seq, indices)
          )
          $ \(~(seq, indices)) ->
            ( bench'
                [title, "size " ++ show size]
                (whnf (\indices -> foldl' func seq indices) indices)
            )
        | size <- take 6 allSizes,
          Updater title sample func <- funcs
      ]

    vectorFolders funcs =
      [ env (pure $ fromList [1 .. size]) $ \vec ->
          ( bench'
              [title, "size " ++ show size]
              (whnf func vec)
          )
        | size <- sizes,
          Folder title fromList func <- funcs
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

sizesLarge :: [Int]
sizesLarge = take 5 allSizes

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
