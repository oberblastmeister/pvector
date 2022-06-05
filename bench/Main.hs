{-# LANGUAGE PackageImports #-}

module Main where

import Control.DeepSeq (NFData)
import Criterion.Main
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (fromJust)
import qualified Data.RRBVector as RRBVector
import qualified Data.Sequence as Seq
import qualified Data.Vector as VB
import qualified "persistent-vector" Data.Vector.Persistent as Vector.Persistent.Other
import qualified "pvector" Data.Vector.Persistent as Vector.Persistent
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

sampleHashMap :: [Int] -> HashMap Int Int
sampleHashMap is = HashMap.fromList $ fmap (\i -> (i, i)) is

snocHashMap :: HashMap Int Int -> Int -> HashMap Int Int
snocHashMap map i = HashMap.insert i i map

main :: IO ()
main =
  defaultMainWith
    defaultConfig
    [
    -- bgroup "snoc" $
    --     snocs
    --       [ Snocer "Data.Vector.Persistent" fromList Vector.Persistent.snoc,
    --         Snocer "Data.Vector.Persistent.Other" Vector.Persistent.Other.fromList Vector.Persistent.Other.snoc
    --         -- Snocer "Data.RRBVector" fromList (RRBVector.|>),
    --         -- Snocer "Data.Vector" fromList VB.snoc,
    --         -- Snocer "Data.HashMap.Strict" sampleHashMap snocHashMap,
    --         -- Snocer "Data.Sequence" fromList (Seq.|>)
    --       ],
      -- bgroup "fromList" $
      --   fromLists
      --     [ FromList "Data.Vector.Persistent" Vector.Persistent.fromList,
      --       FromList "Data.RRBVector" RRBVector.fromList,
      --       FromList "Data.Vector" VB.fromList,
      --       FromList "Data.HashMap.Strict" sampleHashMap,
      --       FromList "Data.Sequence" Seq.fromList
      --     ],
      -- bgroup "map" $
      --   maps
      --     [ Map "Data.Vector.Persistent" fromList (Vector.Persistent.map (20 +)),
      --       Map "Data.Vector.Persistent.Other" Vector.Persistent.Other.fromList (Vector.Persistent.Other.map (20 +)),
      --       Map "Data.RRBVector" fromList (RRBVector.map (20 +))
      --     ],
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
            Folder "Data.RRBVector" Vector.Persistent.fromList sum
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
              (whnf func vec)
          )
        | size <- sizes,
          Folder title fromList func <- funcs
      ]

sizes :: [Int]
sizes = take 4 allSizes

allSizes :: [Int]
allSizes = [10 ^ i | i <- [1 :: Int ..]]
