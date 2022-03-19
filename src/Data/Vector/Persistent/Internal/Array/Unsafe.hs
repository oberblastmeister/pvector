{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Vector.Persistent.Internal.Array.Unsafe where

import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Data (Data)
import Data.Foldable (foldl', toList)
import qualified Data.Foldable as Foldable
import Data.Functor.Classes (Eq1, Show1)
import qualified Data.Functor.Classes
import Data.Primitive.SmallArray
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prelude hiding (length)

data Array a = Array
  { arr :: {-# UNPACK #-} !(SmallArray a),
    len :: {-# UNPACK #-} !Int
  }
  deriving (Data, Typeable, Generic)

data MArray s a = MArray
  { marr :: {-# UNPACK #-} !(SmallMutableArray s a),
    len :: {-# UNPACK #-} !Int
  }
  deriving (Data, Typeable, Generic)

instance Show a => Show (Array a) where
  showsPrec p sa = arrayLiftShowsPrec showsPrec showList p sa

instance Show1 Array where
  liftShowsPrec = arrayLiftShowsPrec

instance Eq a => Eq (Array a) where
  (==) = arrayLiftEq (==)
  {-# INLINE (==) #-}

instance Eq1 Array where
  liftEq = arrayLiftEq
  {-# INLINE liftEq #-}

instance Functor Array where
  fmap f Array {arr, len} = Array {arr = fmap f arr, len}

instance Foldable Array where
  -- Note: we perform the array lookups eagerly so we won't
  -- create thunks to perform lookups even if GHC can't see
  -- that the folding function is strict.
  foldr f = \z !Array {arr, len} ->
    let go i
          | i == len = z
          | (# x #) <- indexSmallArray## arr i =
              f x (go (i + 1))
     in go 0
  {-# INLINE foldr #-}
  foldl f = \z !Array {arr, len} ->
    let go i
          | i < 0 = z
          | (# x #) <- indexSmallArray## arr i =
              f (go (i - 1)) x
     in go (len - 1)
  {-# INLINE foldl #-}
  foldr1 f = \ !Array {arr, len} ->
    let sz = len - 1
        go i =
          case indexSmallArray## arr i of
            (# x #)
              | i == sz -> x
              | otherwise -> f x (go (i + 1))
     in if sz < 0
          then moduleError "foldr1" "Empty SmallArray"
          else go 0
  {-# INLINE foldr1 #-}
  foldl1 f = \ !Array {arr, len} ->
    let sz = len - 1
        go i =
          case indexSmallArray## arr i of
            (# x #)
              | i == 0 -> x
              | otherwise -> f (go (i - 1)) x
     in if sz < 0
          then moduleError "foldl1" "Empty SmallArray"
          else go sz
  {-# INLINE foldl1 #-}
  foldr' f = \z !Array {arr, len} ->
    let go i !acc
          | i == -1 = acc
          | (# x #) <- indexSmallArray## arr i =
              go (i - 1) (f x acc)
     in go (len - 1) z
  {-# INLINE foldr' #-}
  foldl' f = \z !Array {arr, len} ->
    let go i !acc
          | i == len = acc
          | (# x #) <- indexSmallArray## arr i =
              go (i + 1) (f acc x)
     in go 0 z
  {-# INLINE foldl' #-}
  null Array {len} = len == 0
  {-# INLINE null #-}
  length = len
  {-# INLINE length #-}
  maximum Array {arr, len}
    | len == 0 = moduleError "maximum" "Empty SmallArray"
    | (# frst #) <- indexSmallArray## arr 0 = go 1 frst
    where
      go i !e
        | i == len = e
        | (# x #) <- indexSmallArray## arr i = go (i + 1) (max e x)
  {-# INLINE maximum #-}
  minimum Array {arr, len}
    | len == 0 = moduleError "minimum" "Empty SmallArray"
    | (# frst #) <- indexSmallArray## arr 0 = go 1 frst
    where
      go i !e
        | i == len = e
        | (# x #) <- indexSmallArray## arr i = go (i + 1) (min e x)
  {-# INLINE minimum #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}

arrayLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Array a -> ShowS
arrayLiftShowsPrec elemShowsPrec elemListShowsPrec p Array {arr, len} =
  showParen (p > 10) $
    showString "fromListN " . shows len . showString " "
      . listLiftShowsPrec elemShowsPrec elemListShowsPrec 11 (toList arr)

-- this need to be included for older ghcs
listLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> [a] -> ShowS
listLiftShowsPrec _ sl _ = sl

smallArrayLiftCompare :: (a -> b -> Ordering) -> Array a -> Array b -> Ordering
smallArrayLiftCompare elemCompare Array {arr, len} Array {arr = arr', len = len'} = loop 0
  where
    mn = len `min` len'
    loop i
      | i < mn,
        (# x #) <- indexSmallArray## arr i,
        (# x' #) <- indexSmallArray## arr' i =
          elemCompare x x' `mappend` loop (i + 1)
      | otherwise = compare len len'

arrayLiftEq :: (a -> b -> Bool) -> Array a -> Array b -> Bool
arrayLiftEq p Array {arr, len} Array {arr = arr', len = len'} = len == len' && loop (len - 1)
  where
    loop i
      | i < 0 = True
      | (# x #) <- indexSmallArray## arr i,
        (# y #) <- indexSmallArray## arr' i =
          p x y && loop (i - 1)

-- new :: PrimMonad m => Int -> a -> m (MArray (PrimState m) a)
-- new len a = do
--   marr <- newSmallArray len a
--   pure $ MArray {len, marr}
-- {-# INLINE new #-}

-- new_ :: PrimMonad m => Int -> m (MArray (PrimState m) a)
-- new_ n = new n undefinedElem
-- {-# INLINE new_ #-}

-- run :: (forall s. ST s (MArray s a)) -> SmallArray a
-- run act = do
--   runSmallArray act'
--   where
--     act' = marr <$> act

-- create :: Int -> a -> (forall s. SmallMutableArray s a -> ST s ()) -> SmallArray a
-- create = createSmallArray
-- {-# INLINE create #-}

-- create_ :: Int -> (forall s. SmallMutableArray s a -> ST s ()) -> SmallArray a
-- create_ i m = create i (error "impossible") m
-- {-# INLINE create_ #-}

-- copy ::
--   PrimMonad m =>
--   ( SmallMutableArray (PrimState m) a ->
--     Int ->
--     SmallArray a ->
--     Int ->
--     Int ->
--     m ()
--   )
-- copy = copySmallArray
-- {-# INLINE copy #-}

undefinedElem :: a
undefinedElem = error "Data.Vector.Persistent.Internal.Array: Undefined element"
{-# NOINLINE undefinedElem #-}

index :: Array a -> Int -> a
index Array {arr} i = indexSmallArray arr i
{-# INLINE index #-}

write :: PrimMonad m => MArray (PrimState m) a -> Int -> a -> m ()
write MArray {marr} i a = writeSmallArray marr i a
{-# INLINE write #-}

update :: Array a -> Int -> a -> Array a
update Array {arr, len} i a =
  Array
    { arr = runSmallArray $ do
        marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
        writeSmallArray marr i a
        pure marr,
      len
    }
{-# INLINE update #-}

writeResize :: PrimMonad m => MArray (PrimState m) a -> Int -> Int -> a -> m (MArray (PrimState m) a)
writeResize MArray {marr, len} maxSize i a
  | i >= maxSize = error "greater than max size"
  | i >= len = do
      let len' = min maxSize (len * 2)
      marr' <- newSmallArray len' undefinedElem
      copySmallMutableArray marr' 0 marr 0 len
      writeSmallArray marr' i a
      pure MArray {marr = marr', len = len'}
  | otherwise = do writeSmallArray marr i a; pure MArray {marr, len}
{-# INLINE writeResize #-}

updateResize :: Array a -> Int -> Int -> a -> Array a
updateResize array@Array {arr, len} maxSize i a
  | i >= maxSize = error "greater than max size"
  | i >= len = do
      let len' = min maxSize (len + 1)
      let arr' = createSmallArray len' undefinedElem $ \marr -> do
            copySmallArray marr 0 arr 0 len
            writeSmallArray marr i a
      Array {arr = arr', len = len'}
  | otherwise = update array i a
{-# INLINE updateResize #-}

thaw :: PrimMonad m => Array a -> m (MArray (PrimState m) a)
thaw Array {arr, len} = do
  marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
  pure $ MArray {marr, len}
{-# INLINE thaw #-}

empty :: Array a
empty = Array {arr = emptySmallArray, len = 0}
{-# INLINE empty #-}

singleton :: a -> Array a
singleton a =
  Array
    { arr = runSmallArray $ newSmallArray 1 a,
      len = 1
    }
{-# INLINE singleton #-}

fromListN :: Int -> [a] -> Array a
fromListN n as = Array {arr = smallArrayFromListN n as, len = n}
{-# INLINE fromListN #-}

-- snoc :: Array a -> a -> Array a
-- snoc arr a = create_ (len + 1) $ \marr -> do
--   copySmallArray marr 0 arr 0 len
--   writeSmallArray marr len a
--   where
--     len = length arr
-- {-# INLINE snoc #-}

length :: Array a -> Int
length = len
{-# INLINE length #-}

capacity :: Array a -> Int
capacity = sizeofSmallArray . arr
{-# INLINE capacity #-}

moduleError :: String -> String -> a
moduleError fun msg = error ("Data.Vector.Persistent." ++ fun ++ ':' : ' ' : msg)
