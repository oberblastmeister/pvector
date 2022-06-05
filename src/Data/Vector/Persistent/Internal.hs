{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo
-O2
#-}

module Data.Vector.Persistent.Internal where

import Control.Applicative (Alternative, liftA2)
import Control.Applicative qualified
import Control.DeepSeq (NFData (rnf), NFData1)
import Control.DeepSeq qualified
import Control.Monad (MonadPlus)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import Data.Bits (Bits, unsafeShiftL, unsafeShiftR, (.&.))
import Data.Foldable qualified as Foldable
import Data.Foldable.WithIndex (FoldableWithIndex)
import Data.Foldable.WithIndex qualified
import Data.Functor.Classes
  ( Show1,
    liftShowsPrec,
    showsPrec1,
    showsUnaryWith,
  )
import Data.Functor.Identity (Identity, runIdentity)
import Data.Functor.WithIndex (FunctorWithIndex)
import Data.Functor.WithIndex qualified
import Data.Primitive.SmallArray
import Data.Stream.Monadic (Stream (Stream))
import Data.Stream.Monadic qualified as Stream
import Data.Traversable qualified as Traversable
import Data.Traversable.WithIndex (TraversableWithIndex)
import Data.Traversable.WithIndex qualified
import Data.Vector.Persistent.Internal.Array
import Data.Vector.Persistent.Internal.Array qualified as Array
import Data.Vector.Persistent.Internal.Buffer qualified as Buffer
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import GHC.Stack (HasCallStack)
import Prelude hiding (init, length, lookup, map, null, tail)

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
import Test.Inspection
#endif

type role Vector representational

-- invariant: the only time tail can be empty is when init is empty
-- or else tailOffset will give the wrong value
data Vector a = RootNode
  { size :: !Int,
    -- 1 << shift is the maximum that each child can contain
    shift :: !Int,
    init :: !(Array (Node a)),
    tail :: !(Array a)
  }

instance Show1 Vector where
  liftShowsPrec sp sl p v = showsUnaryWith (liftShowsPrec sp sl) "fromList" p (toList v)

instance Show a => Show (Vector a) where
  showsPrec = showsPrec1

instance Eq a => Eq (Vector a) where
  (==) = persistentVectorEq
  {-# INLINE (==) #-}

instance Ord a => Ord (Vector a) where
  compare = persistentVectorCompare
  {-# INLINE compare #-}

instance Functor Vector where
  fmap = Data.Vector.Persistent.Internal.map
  {-# INLINE fmap #-}

instance FunctorWithIndex Int Vector where
  imap = Data.Vector.Persistent.Internal.imap
  {-# INLINE imap #-}

instance Foldable Vector where
  foldr = Data.Vector.Persistent.Internal.foldr
  {-# INLINE foldr #-}
  foldl = Data.Vector.Persistent.Internal.foldl
  {-# INLINE foldl #-}
  foldl' = Data.Vector.Persistent.Internal.foldl'
  {-# INLINE foldl' #-}
  foldr' = Data.Vector.Persistent.Internal.foldr'
  {-# INLINE foldr' #-}
  length = Data.Vector.Persistent.Internal.length
  {-# INLINE length #-}
  null = Data.Vector.Persistent.Internal.null
  {-# INLINE null #-}

instance Traversable Vector where
  traverse = Data.Vector.Persistent.Internal.traverse
  {-# INLINE traverse #-}

instance FoldableWithIndex Int Vector where
  ifoldr = Data.Vector.Persistent.Internal.ifoldr
  {-# INLINE ifoldr #-}
  ifoldl = Data.Vector.Persistent.Internal.ifoldl
  {-# INLINE ifoldl #-}
  ifoldr' = Data.Vector.Persistent.Internal.ifoldr'
  {-# INLINE ifoldr' #-}
  ifoldl' = Data.Vector.Persistent.Internal.ifoldl'
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex Int Vector where
  itraverse = Data.Vector.Persistent.Internal.itraverse
  {-# INLINE itraverse #-}

instance Semigroup (Vector a) where
  (<>) = (><)
  {-# INLINE (<>) #-}

instance Monoid (Vector a) where
  mempty = empty
  {-# INLINE mempty #-}

instance NFData a => NFData (Vector a) where
  rnf RootNode {init, tail} = rnf init `seq` rnf tail
  {-# INLINE rnf #-}

instance Applicative Vector where
  pure = singleton
  {-# INLINE pure #-}
  fs <*> xs = Foldable.foldMap' (\f -> map f xs) fs
  {-# INLINE (<*>) #-}

instance Monad Vector where
  xs >>= f = Foldable.foldMap' f xs
  {-# INLINE (>>=) #-}

instance MonadFail Vector where
  fail _ = empty
  {-# INLINE fail #-}

instance Alternative Vector where
  empty = empty
  {-# INLINE empty #-}
  (<|>) = (><)
  {-# INLINE (<|>) #-}

instance MonadPlus Vector

instance NFData a => NFData (Node a) where
  rnf (DataNode as) = rnf as
  rnf (InternalNode ns) = rnf ns

-- I think this is wrong
instance NFData1 Vector where
  liftRnf f = foldl' (\(_) x -> f x) ()

data Node a
  = InternalNode {getInternalNode :: !(Array (Node a))}
  | DataNode {getDataNode :: !(Array a)}
  deriving (Show)

instance Eq a => Eq (Node a) where
  (==) = nodeEq
  {-# INLINE (==) #-}

instance Ord a => Ord (Node a) where
  compare = nodeCompare
  {-# INLINE compare #-}

instance Show a => IsList (Vector a) where
  type Item (Vector a) = a
  fromList = Data.Vector.Persistent.Internal.fromList
  {-# INLINE fromList #-}
  toList = Data.Vector.Persistent.Internal.toList
  {-# INLINE toList #-}

foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr f z RootNode {init, tail} =
  let z' = Foldable.foldr f z tail
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

streamFoldl' :: (b -> a -> b) -> b -> Vector a -> b
streamFoldl' f z = runIdentity . Stream.foldl' f z . stream
{-# INLINE streamFoldl' #-}

foldl' :: (b -> a -> b) -> b -> Vector a -> b
foldl' f z RootNode {init, tail} =
  let !z' = Foldable.foldl go z init
   in Foldable.foldl f z' tail
  where
    go !z (DataNode as) = Foldable.foldl' f z as
    go !z (InternalNode ns) = Foldable.foldl' go z ns
{-# INLINE foldl' #-}

ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr f z vec@RootNode {size, shift, init, tail}
  | size == 0 = z
  | otherwise =
      let z' = ifoldrStepSmallArray (tailOffset vec) 1 f z tail
       in ifoldrStepSmallArray 0 (1 !<<. shift) (go $! shift - keyBits) z' init
  where
    go _shift i0 (DataNode as) z = ifoldrStepSmallArray i0 1 f z as
    go shift i0 (InternalNode ns) z = ifoldrStepSmallArray i0 (1 !<<. shift) (go $! shift - keyBits) z ns
{-# INLINE ifoldr #-}

ifoldl :: (Int -> b -> a -> b) -> b -> Vector a -> b
ifoldl f z vec@RootNode {size, shift, init, tail}
  | size == 0 = z
  | otherwise =
      let z' = ifoldlStepSmallArray 0 (1 !<<. shift) (go $! shift - keyBits) z init
       in ifoldlStepSmallArray (tailOffset vec) 1 f z' tail
  where
    go _shift i0 z (DataNode as) = ifoldlStepSmallArray i0 1 f z as
    go shift i0 z (InternalNode ns) = ifoldlStepSmallArray i0 (1 !<<. shift) (go $! shift - keyBits) z ns
{-# INLINE ifoldl #-}

ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr' f z vec@RootNode {size, shift, init, tail}
  | size == 0 = z
  | otherwise =
      let !z' = ifoldrStepSmallArray' (tailOffset vec) 1 f z tail
       in ifoldrStepSmallArray' 0 (1 !<<. shift) (go $! shift - keyBits) z' init
  where
    go _shift i0 (DataNode as) !z = ifoldrStepSmallArray' i0 1 f z as
    go shift i0 (InternalNode ns) !z = ifoldrStepSmallArray' i0 (1 !<<. shift) (go $! shift - keyBits) z ns
{-# INLINE ifoldr' #-}

ifoldl' :: (Int -> b -> a -> b) -> b -> Vector a -> b
ifoldl' f z vec@RootNode {size, shift, init, tail}
  | size == 0 = z
  | otherwise =
      let !z' = ifoldlStepSmallArray' 0 (1 !<<. shift) (go $! shift - keyBits) z init
       in ifoldlStepSmallArray' (tailOffset vec) 1 f z' tail
  where
    go _shift i0 !z (DataNode as) = ifoldlStepSmallArray' i0 1 f z as
    go shift i0 !z (InternalNode ns) = ifoldlStepSmallArray' i0 (1 !<<. shift) (go $! shift - keyBits) z ns
{-# INLINE ifoldl' #-}

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

singleton :: a -> Vector a
singleton a = RootNode {size = 1, shift = keyBits, tail = singletonSmallArray a, init = emptySmallArray}
{-# INLINE singleton #-}

empty :: Vector a
empty = RootNode {size = 0, shift = keyBits, init = emptySmallArray, tail = emptySmallArray}
{-# NOINLINE empty #-}

emptyMaxTail :: Vector a
emptyMaxTail =
  RootNode
    { size = 0,
      shift = keyBits,
      init = emptySmallArray,
      tail = runSmallArray $ newSmallArray nodeWidth Array.undefinedElem
    }
{-# NOINLINE emptyMaxTail #-}

null :: Vector a -> Bool
null xs = length xs == 0
{-# INLINE null #-}

(|>) :: Vector a -> a -> Vector a
(|>) = snoc
{-# INLINE (|>) #-}

pattern (:|>) :: Vector a -> a -> Vector a
pattern vec :|> x <-
  (unsnoc -> Just (vec, x))
  where
    vec :|> x = vec `snoc` x

infixl 5 :|>

pattern Empty :: Vector a
pattern Empty <-
  (null -> True)
  where
    Empty = empty

{-# COMPLETE (:|>), Empty #-}

-- | \( O(1) \) Append an element to the end of the vector.
snoc :: Vector a -> a -> Vector a
snoc vec@RootNode {size, tail} a
  -- Room in tail, and vector non-empty
  | (size .&. keyMask) /= 0 =
      vec
        { tail = updateResizeSmallArray tail (size .&. keyMask) a,
          size = size + 1
        }
  | otherwise = snocArr vec 1 $ singletonSmallArray a
{-# INLINEABLE snoc #-}

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
  | otherwise = snocArr vec 1 $ singletonSmallArray a
{-# INLINEABLE unsafeSnoc #-}

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
          init = emptySmallArray
        }
  | size !>>. keyBits > 1 !<<. shift =
      RootNode
        { size = size + addedSize,
          shift = shift + keyBits,
          init =
            let !path = newPath shift tail
                !internal = InternalNode $ init vec
             in twoSmallArray internal path,
          tail = arr
        }
  | otherwise =
      RootNode
        { size = size + addedSize,
          shift,
          init = snocTail size tail shift $ init vec,
          tail = arr
        }
{-# INLINE snocArr #-}

-- This is unsafe because it shrinks the tail in place
-- Shrinks the tail to the amount required by the size
-- This gets rid of any undefined elements
unsafeShrink :: Vector a -> Vector a
unsafeShrink vec@RootNode {size, tail}
  -- we are empty
  | size == 0 = vec {tail = emptySmallArray}
  -- the tail is full, no undefined elements can be present
  | size .&. keyMask == 0 = vec
  -- shrink the tail
  | otherwise = runST $ do
      marr <- unsafeThawSmallArray tail
      shrinkSmallMutableArray marr $ size .&. keyMask
      arr <- unsafeFreezeSmallArray marr
      pure vec {tail = arr}
{-# INLINEABLE unsafeShrink #-}

snocTail :: Int -> Array a -> Int -> Array (Node a) -> Array (Node a)
snocTail size tail = go
  where
    go !level !parent = updateResizeSmallArray parent subIx toInsert
      where
        toInsert
          | level == keyBits = DataNode tail
          | subIx < sizeofSmallArray parent =
              let vec' = indexSmallArray parent subIx
               in InternalNode $ go (level - keyBits) (getInternalNode vec')
          | otherwise = newPath (level - keyBits) tail
        subIx = ((size - 1) !>>. level) .&. keyMask
{-# INLINE snocTail #-}

newPath :: Int -> Array a -> Node a
newPath 0 tail = DataNode tail
newPath level tail = InternalNode $ singletonSmallArray $! newPath (level - keyBits) tail

unsafeIndex :: Vector a -> Int -> a
unsafeIndex vec ix | (# a #) <- Exts.inline unsafeIndex# vec ix = a
{-# NOINLINE unsafeIndex #-}

unsafeIndex# :: Vector a -> Int -> (# a #)
unsafeIndex# vec ix
  | ix >= tailOffset vec = indexSmallArray## (tail vec) (ix .&. keyMask)
  -- no need to use keyMask here as we are at the top
  | otherwise = go ix (shift vec - keyBits) (indexSmallArray (init vec) (ix !>>. shift vec))
  where
    go ix 0 !node = indexSmallArray## (getDataNode node) (ix .&. keyMask)
    go ix level !node = go ix (level - keyBits) (indexSmallArray (getInternalNode node) ix')
      where
        ix' = (ix !>>. level) .&. keyMask
{-# NOINLINE unsafeIndex# #-}

lookup# :: Int -> Vector a -> (# (# #)| a #)
lookup# ix vec
  | (fromIntegral ix :: Word) >= fromIntegral (length vec) = (# (##) | #)
  | otherwise = case Exts.inline unsafeIndex# vec ix of (# x #) -> (# | x #)
{-# NOINLINE lookup# #-}

lookup :: Int -> Vector a -> Maybe a
lookup ix vec
  | (fromIntegral ix :: Word) >= fromIntegral (length vec) = Nothing
  | otherwise = case unsafeIndex# vec ix of (# x #) -> Just x
{-# INLINE lookup #-}

index :: HasCallStack => Int -> Vector a -> a
index ix vec
  | ix < 0 = moduleError "index" $ "negative index: " ++ show ix
  | ix >= length vec = moduleError "index" $ "index too large: " ++ show ix
  | otherwise = Exts.inline unsafeIndex vec ix
{-# INLINEABLE index #-}

(!) :: HasCallStack => Vector a -> Int -> a
(!) = flip index
{-# INLINE (!) #-}

(!?) :: Vector a -> Int -> Maybe a
(!?) = flip lookup
{-# INLINE (!?) #-}

adjust :: (a -> a) -> Int -> Vector a -> Vector a
adjust f = adjust# $ \x -> (# f x #)
{-# INLINE adjust #-}

adjust# :: (a -> (# a #)) -> Int -> Vector a -> Vector a
adjust# f ix vec@RootNode {size, shift, tail}
  -- Invalid index. This funny business uses a single test to determine whether
  -- ix is too small (negative) or too large (at least sz).
  | (fromIntegral ix :: Word) >= fromIntegral size = vec
  | ix >= tailOffset vec = vec {tail = modifySmallArray# tail (ix .&. keyMask) f}
  | otherwise = vec {init = go ix shift (init vec)}
  where
    go ix level vec
      | level == keyBits,
        let !node = DataNode $ modifySmallArray# (getDataNode vec') (ix .&. keyMask) f =
          updateSmallArray vec ix' node
      | otherwise,
        let !node = go ix (level - keyBits) (getInternalNode vec') =
          updateSmallArray vec ix' $! InternalNode node
      where
        ix' = (ix !>>. level) .&. keyBits
        vec' = indexSmallArray vec ix'
{-# INLINE adjust# #-}

adjustIdentity :: (a -> a) -> Int -> Vector a -> Vector a
adjustIdentity f ix vec = runIdentity $ adjustF (pure . f) ix vec
{-# INLINE adjustIdentity #-}

adjustF :: Applicative f => (a -> f a) -> Int -> Vector a -> f (Vector a)
adjustF f ix vec@RootNode {size, shift, tail}
  -- Invalid index. This funny business uses a single test to determine whether
  -- ix is too small (negative) or too large (at least sz).
  | (fromIntegral ix :: Word) >= fromIntegral size = pure vec
  | ix >= tailOffset vec = (\tail -> vec {tail}) <$> modifySmallArrayF tail (ix .&. keyMask) f
  | otherwise = (\init -> vec {init}) <$> go ix shift (init vec)
  where
    go ix level vec
      | level == keyBits =
          (\node' -> updateSmallArray vec ix' $! DataNode node')
            <$> modifySmallArrayF (getDataNode vec') (ix .&. keyMask) f
      | otherwise =
          (\node -> updateSmallArray vec ix' $! InternalNode node)
            <$> go ix (level - keyBits) (getInternalNode vec')
      where
        ix' = (ix !>>. level) .&. keyBits
        vec' = indexSmallArray vec ix'
{-# INLINE adjustF #-}

-- we could use adjust# (\_ -> (# x #)) to implement this
-- and the const like function would get optimized out
-- but we don't because we don't want to create any closures for the go function
-- so we rewrite out the loop and also lambda lift some arguments
-- also: trees are very shallow, so the loop won't be called much.
-- So allocating a closure to not have pass the arguments on the stack has too much overhead
update :: Int -> a -> Vector a -> Vector a
update ix x vec@RootNode {size, shift, tail}
  -- Invalid index. This funny business uses a single test to determine whether
  -- ix is too small (negative) or too large (at least sz).
  | (fromIntegral ix :: Word) >= fromIntegral size = vec
  | ix >= tailOffset vec = vec {tail = updateSmallArray tail (ix .&. keyMask) x}
  | otherwise = vec {init = go ix x shift (init vec)}
  where
    go ix x level vec
      | level == keyBits =
          let !node = DataNode $ updateSmallArray (getDataNode vec') (ix .&. keyMask) x
           in updateSmallArray vec ix' node
      | otherwise =
          let !node = go ix x (level - keyBits) (getInternalNode vec')
           in updateSmallArray vec ix' $! InternalNode node
      where
        ix' = (ix !>>. level) .&. keyMask
        vec' = indexSmallArray vec ix'
{-# INLINEABLE update #-}

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc vec@RootNode {size, tail, init, shift}
  | 0 <- size = Nothing
  -- we need to have this case because we can't run unsnocTail, there is nothing left in the tail
  | 1 <- size, (# x #) <- indexSmallArray## tail 0 = Just (empty, x)
  | nullSmallArray tail',
    (# init', tail' #) <- unsnocTail# size shift init =
      Just (vec {size = size - 1, init = init', tail = tail'}, a)
  | otherwise = Just (vec {size = size - 1, tail = tail'}, a)
  where
    a = lastSmallArray tail
    tail' = popSmallArray tail
{-# INLINEABLE unsnoc #-}

unsnocTail# :: Int -> Int -> Array (Node a) -> (# Array (Node a), Array a #)
unsnocTail# = go
  where
    go size !level !parent
      | level == keyBits = (# popSmallArray parent, getDataNode child #)
      | otherwise = do
          let (# child', tail #) = go size (level - keyBits) (getInternalNode child)
          if nullSmallArray child'
            then (# popSmallArray parent, tail #)
            else (# updateSmallArray parent subIx $ InternalNode child', tail #)
      where
        child = indexSmallArray parent subIx
        -- we need to subtract 2 because the first subtraction gets us to the tail element
        -- the second subtraction gets to the last element in the tree
        subIx = ((size - 2) !>>. level) .&. keyMask
{-# INLINE unsnocTail# #-}

-- | The index of the first element of the tail of the vector (that is, the
-- *last* element of the list representing the tail). This is also the number
-- of elements stored in the array tree.
--
-- Caution: Only gives a sensible result if the vector is nonempty.
tailOffset :: Vector a -> Int
tailOffset vec = (length vec - 1) .&. ((-1) !<<. keyBits)
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

toList :: Vector a -> [a]
toList = pureStreamToList . stream
{-# INLINE toList #-}

-- | Convert a 'Stream' to a list
pureStreamToList :: Stream Identity a -> [a]
pureStreamToList s = Exts.build (\c n -> runIdentity $ Stream.foldr c n s)
{-# INLINE pureStreamToList #-}

map :: (a -> b) -> Vector a -> Vector b
map f vec@RootNode {init, tail} = vec {tail = fmap f tail, init = fmap go init}
  where
    go (DataNode as) = DataNode $ fmap f as
    go (InternalNode ns) = InternalNode $ fmap go ns
{-# INLINE map #-}

imap :: (Int -> a -> b) -> Vector a -> Vector b
imap f vec@RootNode {size, shift, init, tail}
  | size == 0 = empty
  | otherwise =
      vec
        { init = imapStepSmallArray 0 (1 !<<. shift) (go $! shift - keyBits) init,
          tail = imapStepSmallArray (tailOffset vec) 1 f tail
        }
  where
    go _shift i0 (DataNode as) = DataNode $ imapStepSmallArray i0 1 f as
    go shift i0 (InternalNode ns) = InternalNode $ imapStepSmallArray i0 (1 !<<. shift) (go $! shift - keyBits) ns
{-# INLINE imap #-}

traverse :: Applicative f => (a -> f b) -> Vector a -> f (Vector b)
traverse f vec@RootNode {init, tail} =
  liftA2
    (\init tail -> vec {init, tail})
    (Traversable.traverse go init)
    (Traversable.traverse f tail)
  where
    go (DataNode as) = DataNode <$> Traversable.traverse f as
    go (InternalNode ns) = InternalNode <$> Traversable.traverse go ns
{-# INLINE traverse #-}

itraverse :: Applicative f => (Int -> a -> f b) -> Vector a -> f (Vector b)
itraverse f vec@RootNode {size, shift, init, tail}
  | size == 0 = pure empty
  | otherwise =
      liftA2
        (\init tail -> vec {init, tail})
        (itraverseStepSmallArray 0 (1 !<<. shift) (go $! shift - keyBits) init)
        (itraverseStepSmallArray (tailOffset vec) 1 f tail)
  where
    go _shift i0 (DataNode as) = DataNode <$> itraverseStepSmallArray i0 1 f as
    go shift i0 (InternalNode ns) = InternalNode <$> itraverseStepSmallArray i0 (1 !<<. shift) (go $! shift - keyBits) ns
{-# INLINE itraverse #-}

(//) :: Vector a -> [(Int, a)] -> Vector a
(//) vec = Foldable.foldl' (flip $ uncurry update) vec

(><) :: Vector a -> Vector a -> Vector a
(><) vec vec' = foldl' snoc vec vec'

-- | Check the invariant of the vector
invariant :: Vector a -> Bool
invariant _vec = True

fromList :: [a] -> Vector a
fromList = unstream . Stream.fromList

keyBits :: Int
keyBits = 5

nodeWidth :: Int
nodeWidth = 1 !<<. keyBits

keyMask :: Int
keyMask = nodeWidth - 1

(!<<.) :: Bits a => a -> Int -> a
(!<<.) = unsafeShiftL
{-# INLINE (!<<.) #-}

(!>>.) :: Bits a => a -> Int -> a
(!>>.) = unsafeShiftR
{-# INLINE (!>>.) #-}

infixl 8 !<<., !>>.

unstream :: Stream Identity a -> Vector a
unstream stream = runST $ do
  streamToContents stream >>= \case
    (size, tail, [tree]) ->
      pure RootNode {size, shift = keyBits, tail, init = pure tree}
    (size, tail, ls') -> do
      let iterateNodes !shift trees =
            nodes (Prelude.reverse trees) >>= \case
              [tree] -> pure RootNode {size, shift, tail, init = getInternalNode tree}
              trees' -> iterateNodes (shift + keyBits) trees'
      iterateNodes keyBits ls'
  where
    nodes trees = do
      buffer <- Buffer.newWithCapacity nodeWidth
      (buffer, acc) <-
        Foldable.foldlM
          ( \(!buffer, acc) t ->
              if Buffer.length buffer == nodeWidth
                then do
                  result <- Buffer.freeze buffer
                  buffer <- Buffer.push t $ Buffer.clear buffer
                  pure (buffer, InternalNode result : acc)
                else do
                  buffer <- Buffer.push t buffer
                  pure (buffer, acc)
          )
          (buffer, [])
          trees
      final <- Buffer.unsafeFreeze buffer
      pure $ InternalNode final : acc
{-# INLINE unstream #-}

streamToContents :: PrimMonad m => Stream Identity a -> m (Int, SmallArray a, [Node a])
streamToContents (Stream step s) = do
  buffer <- Buffer.newWithCapacity nodeWidth
  loop (0 :: Int) buffer [] s
  where
    loop !size !buffer acc s = do
      case runIdentity $ step s of
        Stream.Yield x s' -> do
          if Buffer.length buffer == nodeWidth
            then do
              result <- Buffer.freeze buffer
              buffer <- Buffer.push x $ Buffer.clear buffer
              loop (size + 1) buffer (DataNode result : acc) s'
            else do
              buffer <- Buffer.push x buffer
              loop (size + 1) buffer acc s'
        Stream.Skip s' -> loop size buffer acc s'
        Stream.Done -> do
          tail <- Buffer.unsafeFreeze buffer
          pure (size, tail, acc)
{-# INLINE streamToContents #-}

stream :: Applicative m => Vector a -> Stream m a
stream = streamL
{-# INLINE stream #-}

streamL :: Applicative m => Vector a -> Stream m a
streamL RootNode {init, tail} = Stream step [(InternalNode init, 0 :: Int), (DataNode tail, 0)]
  where
    step [] = pure Stream.Done
    step ((n, i) : rest) = case n of
      InternalNode ns
        | i >= sizeofSmallArray ns -> pure $ Stream.Skip rest
        | otherwise -> do
            let !(# ns' #) = indexSmallArray## ns i
                !i' = i + 1
            pure $ Stream.Skip $ (ns', 0) : (n, i') : rest
      DataNode xs
        | i >= sizeofSmallArray xs -> pure $ Stream.Skip rest
        | otherwise -> do
            let !(# x #) = indexSmallArray## xs i
                !i' = i + 1
            pure $ Stream.Yield x $ (n, i') : rest
    {-# INLINE step #-}
{-# INLINE streamL #-}

streamR :: Applicative m => Vector a -> Stream m a
streamR RootNode {init, tail} = Stream step [(DataNode tail, tailSize), (InternalNode init, initSize)]
  where
    !tailSize = sizeofSmallArray tail - 1
    !initSize = sizeofSmallArray init - 1

    step [] = pure Stream.Done
    step ((n, i) : rest) = case n of
      InternalNode ns
        | i < 0 -> pure $ Stream.Skip rest
        | otherwise -> do
            let !(# ns' #) = indexSmallArray## ns i
                !i' = i - 1
                !z = nodeLen ns' - 1
            pure $ Stream.Skip $ (ns', z) : (n, i') : rest
      DataNode xs
        | i < 0 -> pure $ Stream.Skip rest
        | otherwise -> do
            let !(# x #) = indexSmallArray## xs i
                !i' = i - 1
            pure $ Stream.Yield x $ (n, i') : rest
    {-# INLINE step #-}
{-# INLINE streamR #-}

nodeLen :: Node a -> Int
nodeLen (InternalNode arr) = sizeofSmallArray arr
nodeLen (DataNode arr) = sizeofSmallArray arr
{-# INLINE nodeLen #-}

streamSumL :: Vector Int -> Int
streamSumL = runIdentity . Stream.foldl' (+) 0 . streamL

streamSumR :: Vector Int -> Int
streamSumR = runIdentity . Stream.foldl' (+) 0 . streamR
