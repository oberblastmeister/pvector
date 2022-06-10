{-# LANGUAGE PackageImports #-}

module Main where

import Control.DeepSeq (NFData)
import Control.Monad.ST (runST)
import Criterion.Main
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.RRBVector qualified as RRBVector
import Data.Sequence qualified as Seq
import Data.Vector.Fusion.Stream.Monadic qualified as Stream
import Data.Vector qualified as VB
import "persistent-vector" Data.Vector.Persistent qualified as Vector.Persistent.Other
import "pvector" Data.Vector.Persistent.Strict qualified as Vector.Persistent
import "pvector" Data.Vector.Persistent.Internal qualified as Vector.Persistent.Internal
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
  Folder :: (NFData (f Int), NFData a) => String -> ([Int] -> f Int) -> (f Int -> a) -> Folder

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
            Snocer "Data.Vector.Persistent.Other" Vector.Persistent.Other.fromList Vector.Persistent.Other.snoc,
            Snocer "Data.RRBVector" fromList (RRBVector.|>),
            Snocer "Data.Vector" fromList VB.snoc,
            Snocer "Data.HashMap.Strict" sampleHashMap snocHashMap,
            Snocer "Data.Sequence" fromList (Seq.|>)
          ],
      bgroup "fromList" $
        fromLists
          [ FromList "Data.Vector.Persistent" Vector.Persistent.fromList,
            FromList "Data.RRBVector" RRBVector.fromList,
            FromList "Data.Vector" VB.fromList,
            FromList "Data.HashMap.Strict" sampleHashMap,
            FromList "Data.Sequence" Seq.fromList
          ],
      bgroup "map" $
        maps
          [ Map "Data.Vector.Persistent" fromList (Vector.Persistent.map (20 +)),
            Map "Data.Vector.Persistent.Other" Vector.Persistent.Other.fromList (Vector.Persistent.Other.map (20 +)),
            Map "Data.RRBVector" fromList (RRBVector.map (20 +))
          ],
      bgroup "index" $
        indexers
          [ Indexer "Data.Vector.Persistent" fromList (\vec i -> fromJust $ Vector.Persistent.lookup i vec),
            Indexer "Data.Vector.Persistent.Other" Vector.Persistent.Other.fromList (\vec i -> fromJust $ Vector.Persistent.Other.index vec i),
            Indexer "Data.RRBVector" fromList (\vec i -> fromJust $ RRBVector.lookup i vec),
            Indexer "Data.Vector" fromList (\vec i -> fromJust $ vec VB.!? i),
            Indexer "Data.HashMap.Strict" sampleHashMap (\map i -> fromJust $ map HashMap.!? i),
            Indexer "Data.Sequence" fromList (\seq i -> fromJust $ seq Seq.!? i)
          ],
      bgroup "update" $
        updaters
          [ Updater "Data.Vector.Persistent" fromList (\vec i -> Vector.Persistent.update i i vec),
            Updater "Data.RRBVector" fromList (\vec i -> RRBVector.update i i vec),
            -- be careful here, ptrEq might cause nothing to be inserted
            Updater "Data.HashMap.Strict" sampleHashMap (\map i -> HashMap.insert i 0 map)
          ],
      bgroup "fold" $
        vectorFolders
          [ Folder "normal" Vector.Persistent.fromList sum,
            -- Folder "toList" Vector.Persistent.fromList (sum . Vector.Persistent.toList),
            -- Folder "streamL" Vector.Persistent.fromList Vector.Persistent.Internal.streamSumL,
            -- Folder "streamR" Vector.Persistent.fromList Vector.Persistent.Internal.streamSumR,
            Folder "Data.RRBVector" RRBVector.fromList sum
          ],
      bgroup "equality" $
        vectorFolders
          [ Folder "normal" Vector.Persistent.fromList (\x -> Vector.Persistent.Internal.persistentVectorEq x x),
            Folder "stream" Vector.Persistent.fromList (\x -> Vector.Persistent.Internal.persistentVectorStreamEq x x)
          ],
      bgroup "compare" $
        vectorFolders
          [ Folder "normal" Vector.Persistent.fromList Vector.Persistent.Internal.persistentVectorCompare
          ]
    ]
  where
    bench' sections = bench $ List.intercalate "/" sections

    -- 1.5x-2x faster than RRBVector
    -- 2x faster than HashMap
    -- way slower than Seq
    snocs funcs =
      [ env (pure $ sample []) $ \seq -> env (pure [1 .. size]) $ \list ->
          ( bench'
              [title, "size " ++ show size]
              (whnf (foldl' func seq) list)
          )
        | size <- sizes,
          Snocer title sample func <- funcs
      ]

    fromLists funcs =
      [ env (pure [1 .. size]) $ \list ->
          (bench' [title, "size " ++ show size] (whnf func list))
        | size <- sizes,
          FromList title func <- funcs
      ]

    maps funcs =
      [ env (pure $ fromList [1 .. size]) $ \vec ->
          (bench' [title, "size " ++ show size] (whnf func vec))
        | size <- sizes,
          Map title fromList func <- funcs
      ]

    -- 1.5x-2x faster than RRBVector
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
        | size <- sizes,
          Updater title sample func <- funcs
      ]

    vectorFolders funcs =
      [ env (pure $ fromList [1 .. size]) $ \vec ->
          ( bench'
              [title, "size " ++ show size]
              (nf func vec)
          )
        | size <- sizes,
          Folder title fromList func <- funcs
      ]

sizes :: [Int]
sizes = take 4 allSizes

allSizes :: [Int]
allSizes = [10 ^ i | i <- [1 :: Int ..]]

-- streamSumL :: Vector.Persistent.Vector Int -> Int
-- streamSumL = runIdentity . Stream.foldl' (+) 0 . streamL

-- streamSumR :: Vector.Persistent.Vector Int -> Int
-- streamSumR = runIdentity . Stream.foldl' (+) 0 . streamR
