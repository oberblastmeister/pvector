module Data.Vector.Persistent.Internal.RAList
  ( RAList,
    cons,
    index,
    empty,
    adjust,
    update,
    adjustF,
    fromList,
  )
where

import Control.DeepSeq (NFData, rnf)
import GHC.Stack (HasCallStack)

newtype RAList a = RAList (List (Pair Int (Tree a)))

instance NFData a => NFData (RAList a) where
  rnf (RAList list) = rnf list

data Pair a b = P !a !b

instance (NFData a, NFData b) => NFData (Pair a b) where
  rnf (P a b) = rnf a `seq` rnf b

-- a spine and value strict list
data List a
  = Cons !a !(List a)
  | Nil

infixr 5 `Cons`

instance NFData a => NFData (List a) where
  rnf Nil = ()
  rnf (x `Cons` xs) = rnf x `seq` rnf xs

-- a spine strict binary tree
data Tree a
  = Leaf a
  | Node a !(Tree a) !(Tree a)

instance NFData a => NFData (Tree a) where
  rnf (Leaf x) = rnf x
  rnf (Node x left right) = rnf left `seq` rnf right `seq` rnf x

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

instance Foldable Tree where
  foldr f z (Leaf x) = f x z
  foldr f z (Node x left right) = f x zl
    where
      zl = foldr f zr left
      zr = foldr f z right
  foldl f z (Leaf x) = f z x
  foldl f z (Node x left right) = f zr x
    where
      zr = foldl f zl right
      zl = foldl f z left

fromList :: [a] -> RAList a
fromList = foldr cons empty

empty :: RAList a
empty = RAList Nil

cons :: a -> RAList a -> RAList a
cons x (RAList list) = RAList $
  case list of
    xss@((P s1 t1) `Cons` (P s2 t2) `Cons` xs) ->
      if s1 == s2
        then (P (1 + 2 * s1) (Node x t1 t2)) `Cons` xs
        else (P 1 (Leaf x)) `Cons` xss
    xs -> (P 1 (Leaf x)) `Cons` xs

index :: Int -> RAList a -> a
index i (RAList list) = go i list
  where
    go _ Nil = error $ "bad"
    go i ((P size tree) `Cons` xs) =
      if i < size
        then indexTree size i tree
        else go (i - size) xs

indexTree :: Int -> Int -> Tree a -> a
indexTree 1 0 (Leaf x) = x
indexTree _ _ (Leaf _) = error "bad"
indexTree size i (Node x left right)
  | i == 0 = x
  | i <= halfSize = indexTree halfSize (i - 1) left
  | otherwise = indexTree halfSize (i - 1 - halfSize) right
  where
    halfSize = size `div` 2

adjust :: (a -> a) -> Int -> RAList a -> RAList a
adjust f i (RAList list) = RAList $ go i list
  where
    go _ Nil = Nil
    go i ((P size tree) `Cons` xs) =
      if i < size
        then (P size (adjustTree f size i tree)) `Cons` xs
        else go (i - size) xs

adjustTree :: (a -> a) -> Int -> Int -> Tree a -> Tree a
adjustTree f 1 0 (Leaf x) = Leaf $ f x
adjustTree _ _ _ tree@(Leaf _) = tree
adjustTree f size i (Node x left right)
  | i == 0 = Node (f x) left right
  | i <= halfSize = adjustTree f halfSize (i - 1) left
  | otherwise = adjustTree f halfSize (i - 1 - halfSize) right
  where
    halfSize = size `div` 2

adjustF :: Applicative f => (a -> f a) -> Int -> RAList a -> f (RAList a)
adjustF f i (RAList list) = RAList <$> go i list
  where
    go _ Nil = pure Nil
    go i ((P size tree) `Cons` xs) =
      if i < size
        then (\tree' -> Cons (P size tree') xs) <$> adjustTreeF f size i tree
        else go (i - size) xs

adjustTreeF :: (Applicative f) => (a -> f a) -> Int -> Int -> Tree a -> f (Tree a)
adjustTreeF f 1 0 (Leaf x) = Leaf <$> f x
adjustTreeF _ _ _ tree@(Leaf _) = pure tree
adjustTreeF f size i (Node x left right)
  | i == 0 = (\x' -> Node x' left right) <$> f x
  | i <= halfSize = adjustTreeF f halfSize (i - 1) left
  | otherwise = adjustTreeF f halfSize (i - 1 - halfSize) right
  where
    halfSize = size `div` 2

update :: a -> Int -> RAList a -> RAList a
update x i (RAList list) = RAList $ go i list
  where
    go _ Nil = error "bad"
    go i ((P size tree) `Cons` xs) =
      if i < size
        then (P size (updateTree x size i tree)) `Cons` xs
        else go (i - size) xs

updateTree :: a -> Int -> Int -> Tree a -> Tree a
updateTree x 1 0 (Leaf _) = Leaf x
updateTree _ _ _ (Leaf _) = error "bad"
updateTree x size i (Node _ left right)
  | i == 0 = Node x left right
  | i <= halfSize = updateTree x halfSize (i - 1) left
  | otherwise = updateTree x halfSize (i - 1 - halfSize) right
  where
    halfSize = size `div` 2

moduleError :: forall a. HasCallStack => String -> String -> a
moduleError fun msg = error ("Data.Vector.Persistent.Internal.RAList" ++ fun ++ ':' : ' ' : msg)
{-# NOINLINE moduleError #-}
