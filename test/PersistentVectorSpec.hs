module PersistentVectorSpec (spec) where

import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Primitive.SmallArray
import Data.Vector.Persistent (Vector)
import qualified Data.Vector.Persistent.Internal as Vector
import Data.Vector.Persistent.Internal.Array
import GHC.Exts (fromList, toList)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  prop "toList fromList identity" $ \(l :: [Int]) ->
    l === toList (fromList @(Vector _) l)

  prop "fmap" fmapProp

  prop "foldr" $ \(l :: [Int]) ->
    foldr (:) [] l === foldr (:) [] (fromList @(Vector _) l)

  prop "foldl" $ \(l :: [Int]) ->
    foldl (flip (:)) [] l === foldl (flip (:)) [] (fromList @(Vector _) l)

  it "update bad" $
    propUpdate
      64
      0
      ((repeat 0 & take 64) ++ [1] ++ (repeat 0 & take 32))

  -- this is somehow broken
  prop "update" propUpdate
  -- prop "update" $ \(ix :: Int) (a :: Int) (l :: [Int]) ->
  --   ix >= 0 ==> do
  --     let arr = fromList @(Array _) l
  --         arr'
  --           | ix >= sizeofSmallArray arr = arr
  --           | otherwise = updateSmallArray arr ix a
  -- toList arr' == toList (Vector.update ix a $ fromList @(Vector _) l)

  prop "traverse" $ \(l :: [Int]) -> do
    let go a = ([a], a)
    fmap toList (traverse go (fromList @(Vector _) l)) === traverse go l

  prop "index" indexProp

  it "index weierd" $ indexProp 9 [1 :: Int .. 15]

  prop "eq self" $ \(l :: [Int]) ->
    fromList @(Vector _) l === fromList l

  prop "eq" $ \(l :: [Int]) (l' :: [Int]) ->
    (l == l') === (fromList @(Vector _) l == fromList @(Vector _) l')

  prop "mappend" $ \(l :: [Int]) (l' :: [Int]) ->
    l <> l' === toList (fromList @(Vector _) l <> fromList @(Vector _) l')

  it "unsnoc bad" $ unsnocProp (replicate 65 0) 1

  prop "unsnoc" unsnocProp

  prop "snoc unsnoc" snocUnsnocProp

  describe "indexed" $ do
    prop "imap" $ \(l :: [Int]) ->
      zip [0 :: Int ..] l === toList (Vector.imap (,) (fromList @(Vector _) l))

propUpdate :: Int -> Int -> [Int] -> Property
propUpdate ix a l =
  ix >= 0 ==> do
    let arr = fromList @(Array _) l
        arr'
          | ix >= sizeofSmallArray arr = arr
          | otherwise = updateSmallArray arr ix a
    toList arr' === toList (Vector.update ix a $ fromList @(Vector _) l)

fmapProp :: [Int] -> Property
fmapProp l = do
  let vec = fmap (+ 20) (fromList @(Vector _) l)
      res = toList vec
  map (+ 20) l === res

snocUnsnocProp :: Int -> Bool
snocUnsnocProp times = do
  Vector.null
    . unsnocTimes
    . snocTimes
    . unsnocTimes
    . snocTimes
    $ Vector.empty
  where
    snocTimes vec = foldl' Vector.snoc vec [1 .. times]
    unsnocTimes vec = foldl' (\vec _ -> unsnoc' vec) vec [1 .. times]

unsnocProp :: [Int] -> Int -> Property
unsnocProp l i = do
  let l' = reverse $ drop i $ reverse l
      vec = fromList @(Vector _) l
      vec' =
        foldl'
          ( \vec _ -> case Vector.unsnoc vec of
              Nothing -> Vector.empty
              Just (vec, _) -> vec
          )
          vec
          [1 .. i]
  l' === toList vec'

unsnoc' :: Vector a -> Vector a
unsnoc' vec = case Vector.unsnoc vec of
  Just (vec, _) -> vec
  _ -> error "empty vector"

indexProp :: Int -> [Int] -> Property
indexProp ix l = do
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
      vec = fromList @(Vector _) l
  indexMaybeList l ix === Vector.lookup ix vec
