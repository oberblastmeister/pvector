{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
#endif

module Data.Vector.Persistent.Internal
  ( module Data.Vector.Persistent.Internal,
  )
where

#ifdef INSPECTION
import Test.Inspection
#endif

import Control.Monad.ST (runST)
import Data.Bits (Bits, unsafeShiftL, unsafeShiftR, (.&.))
import Data.Data
import qualified Data.Foldable as Foldable
import Data.Primitive.SmallArray
import qualified Data.Traversable as Traversable
import Data.Vector.Persistent.Internal.Array (Array)
import qualified Data.Vector.Persistent.Internal.Array as Array
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Prelude hiding (init, length, null, tail)

keyBits :: Int
keyBits = 5

nodeWidth :: Int
nodeWidth = 1 .<<. keyBits

keyMask :: Int
keyMask = nodeWidth - 1

type role Vector nominal

data Vector a = RootNode
  { size :: !Int,
    -- 1 << shift is the maximum that each child can contain
    shift :: !Int,
    init :: !(Array (Node a)),
    tail :: !(Array a)
  }
  deriving (Data, Typeable, Generic)

instance Eq a => Eq (Vector a) where
  (==) = persistentVectorEq
  {-# INLINE (==) #-}

instance Ord a => Ord (Vector a) where
  compare = persistentVectorCompare
  {-# INLINE compare #-}

instance Functor Vector where
  fmap = Data.Vector.Persistent.Internal.map
  {-# INLINE fmap #-}

instance Foldable Vector where
  foldr = Data.Vector.Persistent.Internal.foldr
  {-# INLINE foldr #-}
  foldl = Data.Vector.Persistent.Internal.foldl
  {-# INLINE foldl #-}
  foldl' = Data.Vector.Persistent.Internal.foldl'
  {-# INLINE foldl' #-}
  foldr' = Data.Vector.Persistent.Internal.foldr'
  {-# INLINE foldr' #-}

instance Traversable Vector where
  traverse = Data.Vector.Persistent.Internal.traverse

instance Semigroup (Vector a) where
  (<>) = foldl' snoc
  {-# INLINE (<>) #-}

instance Monoid (Vector a) where
  mappend = (<>)
  {-# INLINE mappend #-}
  mempty = empty
  {-# INLINE mempty #-}

data Node a
  = InternalNode {getInternalNode :: !(Array (Node a))}
  | DataNode {getDataNode :: !(Array a)}
  deriving (Data, Typeable, Generic)

instance Eq a => Eq (Node a) where
  (==) = nodeEq
  {-# INLINE (==) #-}

instance Ord a => Ord (Node a) where
  compare = nodeCompare
  {-# INLINE compare #-}

instance IsList (Vector a) where
  type Item (Vector a) = a
  fromList = Data.Vector.Persistent.Internal.fromList
  toList = Data.Vector.Persistent.Internal.toList

foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr f z RootNode {init, tail} =
  let z' = (Foldable.foldr f z tail)
   in Foldable.foldr go z' init
  where
    go (DataNode as) z = Foldable.foldr f z as
    go (InternalNode ns) z = Foldable.foldr go z ns
{-# INLINE foldr #-}

foldr' :: (a -> b -> b) -> b -> Vector a -> b
foldr' f z RootNode {init, tail} =
  let !z' = Foldable.foldr' f z tail
   in Foldable.foldr' go z' init
  where
    go (DataNode as) !z = Foldable.foldr' f z as
    go (InternalNode ns) !z = Foldable.foldr' go z ns
{-# INLINE foldr' #-}

foldl :: (b -> a -> b) -> b -> Vector a -> b
foldl f z RootNode {init, tail} =
  let z' = Foldable.foldl go z init
   in Foldable.foldl f z' tail
  where
    go z (DataNode as) = Foldable.foldl f z as
    go z (InternalNode ns) = Foldable.foldl go z ns
{-# INLINE foldl #-}

foldl' :: (b -> a -> b) -> b -> Vector a -> b
foldl' f z RootNode {init, tail} =
  let !z' = Foldable.foldl go z init
   in Foldable.foldl f z' tail
  where
    go !z (DataNode as) = Foldable.foldl' f z as
    go !z (InternalNode ns) = Foldable.foldl' go z ns
{-# INLINE foldl' #-}

persistentVectorEq :: Eq a => Vector a -> Vector a -> Bool
persistentVectorEq
  RootNode {size, shift, init, tail}
  RootNode {size = size', shift = shift', init = init', tail = tail'} =
    size == size' && (size == 0 || (shift == shift' && tail == tail' && init == init'))
{-# INLINE persistentVectorEq #-}

nodeEq :: Eq a => Node a -> Node a -> Bool
nodeEq (InternalNode ns) (InternalNode ns') = ns == ns'
nodeEq (DataNode as) (DataNode as') = as == as'
nodeEq _ _ = False
{-# INLINE nodeEq #-}

persistentVectorCompare :: Ord a => Vector a -> Vector a -> Ordering
persistentVectorCompare
  RootNode {size, init, tail}
  RootNode {size = size', init = init', tail = tail'} =
    compare size size'
      <> if size == 0
        then EQ
        else compare init init' <> compare tail tail'
{-# INLINE persistentVectorCompare #-}

nodeCompare :: Ord a => Node a -> Node a -> Ordering
nodeCompare (DataNode as) (DataNode as') = compare as as'
nodeCompare (InternalNode ns) (InternalNode ns') = compare ns ns'
nodeCompare (DataNode _) (InternalNode _) = LT
nodeCompare (InternalNode _) (DataNode _) = GT
{-# INLINE nodeCompare #-}

singleton :: a -> Vector a
singleton a = RootNode {size = 1, shift = keyBits, tail = Array.singleton a, init = Array.empty}
{-# INLINE singleton #-}

empty :: Vector a
empty = RootNode {size = 0, shift = keyBits, init = Array.empty, tail = Array.empty}
{-# NOINLINE empty #-}

emptyMaxTail :: Vector a
emptyMaxTail =
  RootNode
    { size = 0,
      shift = keyBits,
      init = Array.empty,
      tail = Array.run $ Array.new nodeWidth Array.undefinedElem
    }
{-# NOINLINE emptyMaxTail #-}

null :: Vector a -> Bool
null xs = length xs == 0
{-# INLINE null #-}

(.<<.) :: Bits a => a -> Int -> a
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits a => a -> Int -> a
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

infixl 8 .<<., .>>.

(|>) :: Vector a -> a -> Vector a
(|>) = snoc
{-# INLINE (|>) #-}

-- | \( O(1) \) Append an element to the end of the vector.
snoc :: Vector a -> a -> Vector a
snoc vec@RootNode {size, tail} a
  -- Room in tail, and vector non-empty
  | (size .&. keyMask) /= 0 =
      vec
        { tail = Array.updateResize tail (size .&. keyMask) a,
          size = size + 1
        }
  | otherwise = snocArr vec 1 $ Array.singleton a
{-# INLINE snoc #-}

-- Invariant: the tail must be large enough to mutably write to it
-- Best to use this with emptyMaxTail
-- After calling this many times you must run shrink
unsafeSnoc :: Vector a -> a -> Vector a
unsafeSnoc vec@RootNode {size, tail} a
  -- Room in tail, and vector non-empty
  | (size .&. keyMask) /= 0 =
      vec
        { tail =
            -- update the array in place
            runST $ do
              marr <- unsafeThawSmallArray tail
              writeSmallArray marr (size .&. keyMask) a
              unsafeFreezeSmallArray marr,
          size = size + 1
        }
  | otherwise = snocArr vec 1 $
      Array.create nodeWidth Array.undefinedElem $ \marr ->
        Array.write marr 0 a
{-# INLINE unsafeSnoc #-}

-- Invariant: arr cannot be empty
snocArr ::
  -- | The Vector to perform the operation on
  Vector a ->
  -- | The the added size. We can't find this from the array because the array might have bogus size due to undefined elements
  Int ->
  -- | The array to add as the new tail
  Array a ->
  Vector a
snocArr vec@RootNode {size, shift, tail} addedSize arr
  | null vec =
      RootNode
        { size = addedSize,
          shift = keyBits,
          tail = arr,
          init = Array.empty
        }
  | size .>>. keyBits > 1 .<<. shift =
      RootNode
        { size = size + addedSize,
          shift = shift + keyBits,
          init =
            let !path = newPath shift tail
             in Array.fromListN 2 [InternalNode (init vec), path],
          tail = arr
        }
  | otherwise =
      RootNode
        { size = size + addedSize,
          shift,
          init = pushTail size tail shift $ init vec,
          tail = arr
        }
{-# INLINE snocArr #-}

-- Shrinks the tail to the amount required by the size
-- This gets rid of any undefined elements
unsafeShrink :: Vector a -> Vector a
unsafeShrink vec@RootNode {size, tail}
  -- we are empty
  | size == 0 = vec {tail = Array.empty}
  -- the tail is full, no undefined elements can be present
  | size .&. keyMask == 0 = vec
  -- shrink the tail
  | otherwise = runST $ do
      marr <- Array.unsafeThaw tail
      Array.shrink marr $ size .&. keyMask
      arr <- Array.unsafeFreeze marr
      pure vec {tail = arr}
{-# INLINE unsafeShrink #-}

pushTail :: Int -> Array a -> Int -> Array (Node a) -> Array (Node a)
pushTail size tail = go
  where
    go !level !parent = Array.updateResize parent subIx toInsert
      where
        toInsert
          | level == keyBits = DataNode tail
          | subIx < Array.length parent =
              let vec' = Array.index parent subIx
               in InternalNode $ go (level - keyBits) (getInternalNode vec')
          | otherwise = newPath (level - keyBits) tail

        subIx = ((size - 1) .>>. level) .&. keyMask
{-# INLINE pushTail #-}

newPath :: Int -> Array a -> Node a
newPath 0 tail = DataNode tail
newPath level tail = InternalNode $ Array.singleton $! newPath (level - keyBits) tail

unsafeIndex :: Vector a -> Int -> a
unsafeIndex vec ix | (# a #) <- unsafeIndex# vec ix = a
{-# INLINE unsafeIndex #-}

unsafeIndex# :: Vector a -> Int -> (# a #)
unsafeIndex# vec ix
  | ix >= tailOffset vec = Array.index# (tail vec) (ix .&. keyMask)
  -- no need to use keyMask here as we are at the top
  | otherwise = go (shift vec - keyBits) (Array.index (init vec) (ix .>>. shift vec))
  where
    go 0 !node = Array.index# (getDataNode node) (ix .&. keyMask)
    go level !node = go (level - keyBits) (Array.index (getInternalNode node) ix')
      where
        ix' = (ix .>>. level) .&. keyMask
{-# INLINE unsafeIndex# #-}

indexMaybe :: Vector a -> Int -> Maybe a
indexMaybe vec ix
  -- Check if the index is valid. This funny business uses a single test to
  -- determine whether ix is too small (negative) or too large (at least the
  -- length of the vector).
  | (fromIntegral ix :: Word) < fromIntegral (length vec) =
      Just $ unsafeIndex vec ix
  | otherwise = Nothing
{-# INLINE indexMaybe #-}

index :: Vector a -> Int -> a
index vec ix
  | ix < 0 = moduleError "index" $ "negative index: " ++ show ix
  | ix >= length vec = moduleError "index" $ "index too large: " ++ show ix
  | otherwise = unsafeIndex vec ix
{-# INLINE index #-}

(!) :: Vector a -> Int -> a
(!) = index
{-# INLINE (!) #-}

(!?) :: Vector a -> Int -> Maybe a
(!?) = indexMaybe
{-# INLINE (!?) #-}

modify :: Vector a -> Int -> (a -> a) -> Vector a
modify vec@RootNode {size, shift, tail} ix f
  -- Invalid index. This funny business uses a single test to determine whether
  -- ix is too small (negative) or too large (at least sz).
  | (fromIntegral ix :: Word) >= fromIntegral size = vec
  | ix >= tailOffset vec = vec {tail = Array.modify tail (ix .&. keyMask) f}
  | otherwise = vec {init = go shift (init vec)}
  where
    go level vec
      | level == keyBits,
        let !node = DataNode $ Array.modify (getDataNode vec') (ix .&. keyMask) f =
          Array.update vec ix' node
      | otherwise,
        let !node = go (level - keyBits) (getInternalNode vec') =
          Array.update vec ix' $! InternalNode node
      where
        ix' = (ix .>>. level) .&. keyBits
        vec' = Array.index vec ix'
{-# INLINE modify #-}

update :: Vector a -> Int -> a -> Vector a
update vec@RootNode {size, shift, tail} ix a
  -- Invalid index. This funny business uses a single test to determine whether
  -- ix is too small (negative) or too large (at least sz).
  | (fromIntegral ix :: Word) >= fromIntegral size = vec
  | ix >= tailOffset vec = vec {tail = Array.update tail (ix .&. keyMask) a}
  | otherwise = vec {init = go shift (init vec)}
  where
    go level vec
      | level == keyBits,
        let !node = DataNode $ Array.update (getDataNode vec') (ix .&. keyMask) a =
          Array.update vec ix' node
      | otherwise,
        let !node = go (level - keyBits) (getInternalNode vec') =
          Array.update vec ix' $! InternalNode node
      where
        ix' = (ix .>>. level) .&. keyBits
        vec' = Array.index vec ix'
{-# INLINE update #-}

-- unsnoc :: Vector a -> Maybe (a, Vector a)
-- unsnoc vec@RootNode {size, shift, tail}
--   | size == 0 = Nothing
--   | ix >= tailOffset vec =
--       Just
--         ( Array.index tail (ix .&. keyMask),
--           vec
--             { size = size - 1,
--               tail = Array.update tail (ix .&. keyMask) Array.undefinedElem
--             }
--         )
--   | otherwise = undefined
--   where
--     go level vec
--       | level == keyBits,
--         let !node = DataNode $ Array.update (getDataNode vec') (ix .&. keyMask) Array.undefinedElem =
--           Array.update vec ix' node
--       | otherwise,
--         let !node = go (level - keyBits) (getInternalNode vec') =
--           Array.update vec ix' $! InternalNode node
--       where
--         ix' = (ix .>>. level) .&. keyBits
--         vec' = Array.index vec ix'
--     ix = size - 1

-- | The index of the first element of the tail of the vector (that is, the
-- *last* element of the list representing the tail). This is also the number
-- of elements stored in the array tree.
--
-- Caution: Only gives a sensible result if the vector is nonempty.
tailOffset :: Vector a -> Int
tailOffset v = (length v - 1) .&. ((-1) .<<. keyBits)
{-# INLINE tailOffset #-}

-- | \( O(1) \) Get the length of the vector.
length :: Vector a -> Int
length = size
{-# INLINE length #-}

impossibleError :: forall a. a
impossibleError = error "this should be impossible"

moduleError :: forall a. HasCallStack => String -> String -> a
moduleError fun msg = error ("Data.Vector.Persistent.Internal" ++ fun ++ ':' : ' ' : msg)
{-# NOINLINE moduleError #-}

fromList :: [a] -> Vector a
fromList xs = unsafeShrink $ Foldable.foldl' unsafeSnoc emptyMaxTail xs
{-# INLINE fromList #-}

reverse :: Vector a -> Vector a
reverse vec = unsafeShrink $ foldr' (flip unsafeSnoc) emptyMaxTail vec
{-# INLINE reverse #-}

filter :: (a -> Bool) -> Vector a -> Vector a
filter f vec = foldl' (\vec x -> if f x then unsafeSnoc vec x else vec) emptyMaxTail vec
{-# INLINE filter #-}

partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition f vec = case twoVec of
  TwoVec vec1 vec2 -> (unsafeShrink vec1, unsafeShrink vec2)
  where
    twoVec = foldl' go (TwoVec emptyMaxTail emptyMaxTail) vec
    go (TwoVec vec1 vec2) x =
      if f x
        then TwoVec (unsafeSnoc vec1 x) vec2
        else TwoVec vec1 (unsafeSnoc vec2 x)
{-# INLINE partition #-}

data TwoVec a = TwoVec {-# UNPACK #-} !(Vector a) {-# UNPACK #-} !(Vector a)

toList :: Vector a -> [a]
toList = Foldable.toList
{-# INLINE toList #-}

map :: (a -> b) -> Vector a -> Vector b
map f vec@RootNode {init, tail} = vec {tail = tail', init = init'}
  where
    tail' = fmap f tail
    init' = fmap go init

    go (DataNode as) = DataNode $ fmap f as
    go (InternalNode ns) = InternalNode $ fmap go ns
{-# INLINE map #-}

traverse :: Applicative f => (a -> f b) -> Vector a -> f (Vector b)
traverse f vec@RootNode {init, tail} =
  (\init' tail' -> vec {init = init', tail = tail'})
    <$> (Traversable.traverse go init)
    <*> (Traversable.traverse f tail)
  where
    go (DataNode as) = DataNode <$> Traversable.traverse f as
    go (InternalNode ns) = InternalNode <$> Traversable.traverse go ns
{-# INLINE traverse #-}

#ifdef INSPECTION

update' :: Vector a -> Int -> a -> Vector a
update' vec ix a = modify vec ix $ const a
{-# INLINE update #-}

inspect $ 'update === 'update'
#endif
