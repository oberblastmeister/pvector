{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary where

import Data.Vector.Persistent (Vector)
import qualified Data.Vector.Persistent as PVec
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = PVec.fromList <$> arbitrary
  shrink = map PVec.fromList . shrink . PVec.toList
