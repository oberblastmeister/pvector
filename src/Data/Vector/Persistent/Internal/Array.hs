{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Vector.Persistent.Internal.Array
  ( Array,
    MArray,
    nullSmallArray,
    lastSmallArray#,
    singletonSmallArray,
    twoSmallArray,
    updateSmallArray,
    modifySmallArray,
    modifySmallArrayF,
    modifySmallArray',
    updateResizeSmallArray,
    popSmallArray,
    undefinedElem,
    ifoldrStepSmallArray,
    ifoldlStepSmallArray,
    ifoldrStepSmallArray',
    ifoldlStepSmallArray',
    imapStepSmallArray,
    imapStepSmallArray',
    itraverseStepSmallArray,
    modifySmallArray#,
    mapSmallArray#,
    shrinkSmallMutableArray_,
    snocSmallArray,
    unsnocSmallArray,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST, runST)
import Data.Coerce (coerce)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import qualified Data.Primitive as Primitive
import Data.Primitive.SmallArray
import GHC.Exts (SmallMutableArray#)

type Array = SmallArray

type MArray = SmallMutableArray

-- | Used to support older ghcs.
shrinkSmallMutableArray_ :: PrimMonad m => MArray (PrimState m) a -> Int -> m (MArray (PrimState m) a)
#if __GLASGOW_HASKELL__ >= 810
shrinkSmallMutableArray_ marr n = Primitive.shrinkSmallMutableArray marr n $> marr
#else
shrinkSmallMutableArray_ mary n = Primitive.cloneSmallMutableArray mary 0 n
#endif 
{-# INLINE shrinkSmallMutableArray_ #-}

mapSmallArray# :: (a -> (# b #)) -> SmallArray a -> SmallArray b
mapSmallArray# f sa = createSmallArray (length sa) (error "mapSmallArray#") $ \smb -> do
  let go i =
        when (i < length sa) $ do
          x <- indexSmallArrayM sa i
          let !(# y #) = f x
          writeSmallArray smb i y *> go (i + 1)
  go 0
{-# INLINE mapSmallArray# #-}

nullSmallArray :: SmallArray a -> Bool
nullSmallArray arr = sizeofSmallArray arr == 0
{-# INLINE nullSmallArray #-}

unsnocSmallArray :: SmallArray a -> Maybe (SmallArray a, a)
unsnocSmallArray arr = do
  let len = sizeofSmallArray arr
  when (len == 0) Nothing
  x <- indexSmallArrayM arr (len - 1)
  let !arr' = cloneSmallArray arr 0 (len - 1)
  pure (arr', x)
{-# INLINE unsnocSmallArray #-}

lastSmallArray# :: SmallArray a -> (# a #)
lastSmallArray# arr = indexSmallArray## arr $ sizeofSmallArray arr

singletonSmallArray :: a -> Array a
singletonSmallArray a = runSmallArray $ newSmallArray 1 a
{-# INLINE singletonSmallArray #-}

twoSmallArray :: a -> a -> Array a
twoSmallArray x y = runSmallArray $ do
  marr <- newSmallArray 2 x
  writeSmallArray marr 1 y
  pure marr
{-# INLINE twoSmallArray #-}

updateSmallArray :: Array a -> Int -> a -> Array a
updateSmallArray arr i x = modifySmallArray# arr i $ \_ -> (# x #)
{-# INLINE updateSmallArray #-}

modifySmallArray :: Array a -> Int -> (a -> a) -> Array a
modifySmallArray arr i f = modifySmallArray# arr i $ \x -> (# f x #)
{-# INLINE modifySmallArray #-}

modifySmallArrayF :: Functor f => Array a -> Int -> (a -> f a) -> f (Array a)
modifySmallArrayF arr i f | (# x #) <- indexSmallArray## arr i = updateSmallArray arr i <$> f x
{-# INLINE modifySmallArrayF #-}

modifySmallArray' :: Array a -> Int -> (a -> a) -> Array a
modifySmallArray' arr i f = modifySmallArray# arr i $ \x -> let !x' = f x in (# x' #)
{-# INLINE modifySmallArray' #-}

modifySmallArray# :: Array a -> Int -> (a -> (# a #)) -> Array a
modifySmallArray# arr i f = runSmallArray $ do
  marr <- thawSmallArray arr 0 $ sizeofSmallArray arr
  x <- indexSmallArrayM arr i
  let !(# x' #) = f x
  writeSmallArray marr i x'
  pure marr
{-# INLINE modifySmallArray# #-}

snocSmallArray :: Array a -> a -> Array a
snocSmallArray arr x = runSmallArray $ do
  marr <- newSmallArray (sizeofSmallArray arr + 1) x
  copySmallArray marr 0 arr 0 (sizeofSmallArray arr)
  pure marr
{-# INLINE snocSmallArray #-}

-- this is wrong
updateResizeSmallArray :: Array a -> Int -> a -> Array a
updateResizeSmallArray arr i a =
  if i == len
    then snocSmallArray arr a
    else updateSmallArray arr i a
  where
    len = sizeofSmallArray arr
{-# INLINE updateResizeSmallArray #-}

popSmallArray :: Array a -> Array a
popSmallArray arr = runSmallArray $ thawSmallArray arr 0 (sizeofSmallArray arr - 1)
{-# INLINE popSmallArray #-}

undefinedElem :: forall a. a
undefinedElem = error "undefined element"
{-# NOINLINE undefinedElem #-}

ifoldrStepSmallArray :: Int -> Int -> (Int -> a -> b -> b) -> b -> SmallArray a -> b
ifoldrStepSmallArray i0 step f z arr = do
  let len = sizeofSmallArray arr
      go i j
        | i == len = z
        | (# x #) <- indexSmallArray## arr i = f j x (go (i + 1) $! j + step)
  go 0 i0
{-# INLINE ifoldrStepSmallArray #-}

ifoldlStepSmallArray :: Int -> Int -> (Int -> b -> a -> b) -> b -> SmallArray a -> b
ifoldlStepSmallArray i0 step f z arr = do
  let len = sizeofSmallArray arr
      go i j
        | i < 0 = z
        | (# x #) <- indexSmallArray## arr i = f j (go (i - 1) $! j - step) x
  go (len - 1) i0
{-# INLINE ifoldlStepSmallArray #-}

ifoldrStepSmallArray' :: Int -> Int -> (Int -> a -> b -> b) -> b -> SmallArray a -> b
ifoldrStepSmallArray' i0 step f z arr = do
  let go i j acc
        | i < 0 = acc
        | (# x #) <- indexSmallArray## arr i = (go (i - 1) $! (j - step)) $! f j x acc
  go (sizeofSmallArray arr) i0 z
{-# INLINE ifoldrStepSmallArray' #-}

ifoldlStepSmallArray' :: Int -> Int -> (Int -> b -> a -> b) -> b -> SmallArray a -> b
ifoldlStepSmallArray' i0 step f z arr = do
  let go i j acc
        | i == sizeofSmallArray arr = acc
        | (# x #) <- indexSmallArray## arr i = (go (i + 1) $! (j + step)) $! f j acc x
  go 0 i0 z
{-# INLINE ifoldlStepSmallArray' #-}

imapStepSmallArray :: Int -> Int -> (Int -> a -> b) -> SmallArray a -> SmallArray b
imapStepSmallArray i0 step f arr = createSmallArray len undefinedElem $ \marr -> do
  let go i k = when (i < len) $ do
        x <- indexSmallArrayM arr i
        writeSmallArray marr i (f k x)
        go (i + 1) $! k + step
  go 0 i0
  where
    len = sizeofSmallArray arr
{-# INLINE imapStepSmallArray #-}

imapStepSmallArray' :: Int -> (a -> Int) -> (Int -> a -> b) -> SmallArray a -> SmallArray b
imapStepSmallArray' i0 step f arr = createSmallArray len undefinedElem $ \marr -> do
  let go i k = when (i < len) $ do
        x <- indexSmallArrayM arr i
        writeSmallArray marr i $! f k x
        go (i + 1) $! k + step x
  go 0 i0
  where
    len = sizeofSmallArray arr
{-# INLINE imapStepSmallArray' #-}

newtype STA a = STA {_runSTA :: forall s. SmallMutableArray# s a -> ST s (SmallArray a)}

runSTA :: Int -> STA a -> SmallArray a
runSTA !sz = \(STA m) ->
  runST $
    newSmallArray sz undefinedElem
      >>= \(SmallMutableArray ar#) -> m ar#

itraverseStepSmallArray :: Applicative f => Int -> Int -> (Int -> a -> f b) -> SmallArray a -> f (SmallArray b)
itraverseStepSmallArray i0 step f = \ !arr -> do
  let len = sizeofSmallArray arr
      go i k
        | i == len =
            pure $ STA $ \marr -> unsafeFreezeSmallArray (SmallMutableArray marr)
        | (# x #) <- indexSmallArray## arr i =
            liftA2
              (\b (STA m) -> STA $ \marr -> writeSmallArray (SmallMutableArray marr) i b >> m marr)
              (f k x)
              (go (i + 1) $! k + step)
  if len == 0
    then pure emptySmallArray
    else runSTA len <$> go 0 i0
{-# INLINE [1] itraverseStepSmallArray #-}

{-# RULES
"itraverseStepSmallArray/ST" forall i0 step (f :: Int -> a -> ST s b).
  itraverseStepSmallArray i0 step f =
    itraverseStepSmallArrayP i0 step f
"itraverseStepSmallArray/IO" forall i0 step (f :: Int -> a -> IO b).
  itraverseStepSmallArray i0 step f =
    itraverseStepSmallArrayP i0 step f
"itraverseStepSmallArray/Id" forall i0 step (f :: Int -> a -> Identity b).
  itraverseStepSmallArray i0 step f =
    ( coerce ::
        (SmallArray a -> SmallArray (Identity b)) ->
        SmallArray a ->
        Identity (SmallArray b)
    )
      (imapStepSmallArray i0 step f)
  #-}

-- | This is the fastest, most straightforward way to traverse
-- an array, but it only works correctly with a sufficiently
-- "affine" 'PrimMonad' instance. In particular, it must only produce
-- /one/ result array. 'Control.Monad.Trans.List.ListT'-transformed
-- monads, for example, will not work right at all.
itraverseStepSmallArrayP :: PrimMonad m => Int -> Int -> (Int -> a -> m b) -> SmallArray a -> m (SmallArray b)
itraverseStepSmallArrayP i0 step f = \ !ary -> do
  let len = sizeofSmallArray ary
      go i k marr
        | i == len = unsafeFreezeSmallArray marr
        | otherwise = do
            a <- indexSmallArrayM ary i
            b <- f k a
            writeSmallArray marr i b
            (go (i + 1) $! k + step) marr
  marr <- newSmallArray len undefinedElem
  go 0 i0 marr
{-# INLINE itraverseStepSmallArrayP #-}
