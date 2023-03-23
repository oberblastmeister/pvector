{-# LANGUAGE TemplateHaskell #-}

module Properties where

import Arbitrary ()
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Data.Vector.Persistent (Vector)
import qualified Data.Vector.Persistent as PVec
import Test.QuickCheck
import Test.QuickCheck.Classes.Base
import Test.Tasty
import Test.Tasty.QuickCheck

prop_toList_fromList_identity :: [Int] -> Property
prop_toList_fromList_identity l = l === PVec.toList (PVec.fromList l)

prop_fmap :: Fun Int Int -> [Int] -> Property
prop_fmap (applyFun -> f) l = map f l === PVec.toList (fmap f (PVec.fromList l))

prop_update :: NonNegative Int -> Int -> [Int] -> Property
prop_update (NonNegative ix) a l =
  toList (Seq.update ix a $ Seq.fromList l)
    === toList (PVec.update ix a $ PVec.fromList l)

prop_adjust :: Fun Int Int -> NonNegative Int -> [Int] -> Property
prop_adjust (applyFun -> f) (NonNegative i) l =
  toList (Seq.adjust f i $ Seq.fromList l)
    === PVec.toList (PVec.adjust f i $ PVec.fromList l)

prop_foldr :: [Int] -> Property
prop_foldr l = foldr (:) [] l === foldr (:) [] (PVec.fromList l)

prop_foldl :: [Int] -> Property
prop_foldl l = foldl (flip (:)) [] l === foldl (flip (:)) [] (PVec.fromList l)

prop_traverse :: [Int] -> Property
prop_traverse l = do
  let go a = ([a], a)
  fmap toList (traverse go (PVec.fromList l)) === traverse go l

prop_index :: NonNegative Int -> [Int] -> Property
prop_index (NonNegative i) l = Seq.lookup i (Seq.fromList l) === PVec.lookup i (PVec.fromList l)

prop_mappend :: [Int] -> [Int] -> Property
prop_mappend l1 l2 = PVec.toList (PVec.fromList l1 <> PVec.fromList l2) === l1 <> l2

prop_snoc :: [Int] -> Int -> Property
prop_snoc l a = PVec.toList (PVec.fromList l `PVec.snoc` a) === l ++ [a]

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (a : as) = case unsnoc as of
  Nothing -> Just ([], a)
  Just (as', a') -> Just (a : as', a')

prop_unsnoc :: [Int] -> Property
prop_unsnoc l = unsnoc l === fmap (first PVec.toList) (PVec.unsnoc (PVec.fromList l))

$(return [])

tests :: TestTree
tests =
  testGroup
    "Properties"
    [ testProperties "PersistentVector" $allProperties,
      testGroup "Laws" $ testLaws <$> laws
    ]
  where
    laws =
      [ eqLaws p,
        ordLaws p,
        isListLaws p,
        semigroupLaws p,
        monoidLaws p,
        showLaws p,
        functorLaws p',
        applicativeLaws p',
        monadLaws p',
        foldableLaws p',
        traversableLaws p',
        alternativeLaws p',
        monadPlusLaws p'
      ]
    p = Proxy @(Vector Int)
    p' = Proxy @Vector

testLaws :: Laws -> TestTree
testLaws (Laws name pairs) = testGroup name (map (uncurry testProperty) pairs)
