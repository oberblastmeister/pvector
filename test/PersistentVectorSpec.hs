{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PersistentVectorSpec (spec) where

import Data.Vector.Persistent (Vector)
import qualified Data.Vector.Persistent.Internal as Vector
import Data.Vector.Persistent.Internal.Array (Array)
import qualified Data.Vector.Persistent.Internal.Array as Array
import Debug.Trace (trace)
import GHC.Exts (fromList, toList)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((==>))

spec :: Spec
spec = parallel $ do
  prop "toList fromList identity" $ \(l :: [Int]) ->
    l == toList (fromList @(Vector _) l)

  prop "fmap" $ \(l :: [Int]) ->
    map (+ 20) l == toList (fmap (+ 20) (fromList @(Vector _) l))

  prop "foldr" $ \(l :: [Int]) ->
    foldr (:) [] l == foldr (:) [] (fromList @(Vector _) l)

  prop "foldl" $ \(l :: [Int]) ->
    foldl (flip (:)) [] l == foldl (flip (:)) [] (fromList @(Vector _) l)

  prop "update" $ \(ix :: Int) (a :: Int) (l :: [Int]) ->
    ix >= 0 ==> do
      let arr = fromList @(Array _) l
          arr'
            | ix >= Array.length arr = arr
            | otherwise = Array.update arr ix a
      toList arr' == toList (Vector.update (fromList @(Vector _) l) ix a)

  prop "traverse" $ \(l :: [Int]) -> do
    let go a = ([a], a)
    fmap toList (traverse go (fromList @(Vector _) l)) == traverse go l

  prop "index" $ \(ix :: Int) (l :: [Int]) -> do
    let indexMaybeList :: [a] -> Int -> Maybe a
        indexMaybeList xs n
          | n < 0 = Nothing
          -- Definition adapted from GHC.List
          | otherwise =
              foldr
                ( \x r k -> case k of
                    0 -> Just x
                    _ -> r (k - 1)
                )
                (const Nothing)
                xs
                n
    indexMaybeList l ix == Vector.indexMaybe (fromList @(Vector _) l) ix

  prop "eq" $ \(l :: [Int]) (l' :: [Int]) ->
    (l == l') == (fromList @(Vector _) l == fromList @(Vector _) l')

  prop "mappend" $ \(l :: [Int]) (l' :: [Int]) ->
    l <> l' == toList (fromList @(Vector _) l <> fromList @(Vector _) l')

-- prop "unsnoc" $ \(l :: [Int]) (i :: Int) -> do
--   let l' =
--         ( foldl'
--             ( \l _ -> case l of
--                 [] -> []
--                 _ : xs -> xs
--             )
--             l
--             [0 .. i]
--         )
--       vec = fromList @(Vector _) l
--       vec' =
--         foldl'
--           ( \vec _ -> case Vector.unsnoc vec of
--               Nothing -> Vector.empty
--               Just (vec, _) -> vec
--           )
--           vec
--           [0 .. i]
--   l' == toList vec'
