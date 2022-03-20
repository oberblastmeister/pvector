{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Vector.Persistent.Internal
  ( module Data.Vector.Persistent.Internal,
  )
where

import Data.Bits (Bits, unsafeShiftL, unsafeShiftR, (.&.))
import Data.Data
import Data.Vector.Persistent.Internal.Array (Array)
import qualified Data.Vector.Persistent.Internal.Array as Array
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Prelude hiding (init, length, null, tail)

keyBits :: Int
keyBits = 5

nodeWidth :: Int
nodeWidth = 2 ^ keyBits

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

data Node a
  = InternalNode {getInternalNode :: !(Array (Node a))}
  | DataNode {getDataNode :: !(Array a)}
  deriving (Data, Typeable, Generic)

instance Eq a => Eq (Node a) where
  (==) = nodeEq
  {-# INLINE (==) #-}

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

-- | \( O(1) \) Construct a vector with a single element.
singleton :: a -> Vector a
singleton a =
  RootNode
    { size = 1,
      shift = keyBits,
      tail = Array.singleton a,
      init = Array.empty
    }

empty :: Vector a
empty = RootNode {size = 0, shift = keyBits, init = Array.empty, tail = Array.empty}

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

-- | \( O(1) \) Append an element to the end of the vector.
snoc :: Vector a -> a -> Vector a
snoc vec@RootNode {size, tail} a
  -- Room in tail, and vector non-empty
  | (size .&. keyMask) /= 0 = vec {tail = Array.update tail (size .&. keyMask) a, size = size + 1}
  | otherwise = snocMain vec a
{-# INLINE snoc #-}

snocMain :: Vector a -> a -> Vector a
snocMain vec a | null vec = singleton a
snocMain vec@RootNode {size, shift, tail} a
  -- Overflow current root
  -- how much the root can contain
  -- _ > how much the each children can contain
  -- how much each children can contain times number of children (how much the root can contain)
  --   (1 `shiftL` sh) * 32
  -- how much each children has right now > how much each children can contain
  | size .>>. keyBits > 1 .<<. shift =
      RootNode
        { size = size + 1,
          shift = shift + keyBits,
          init =
            let !path = newPath shift tail
             in Array.fromListN 2 [InternalNode (init vec), path],
          tail = Array.singleton a
        }
  -- Insert into the tree
  | otherwise =
      RootNode
        { size = size + 1,
          shift,
          tail = Array.singleton a,
          init = pushTail size tail shift (init vec)
        }

pushTail :: Int -> Array a -> Int -> Array (Node a) -> Array (Node a)
pushTail size tail = go
  where
    go !level !parent = Array.update parent subIx toInsert
      where
        toInsert
          | level == keyBits = DataNode tail
          | subIx < Array.length parent =
              let children = getInternalNode $ Array.index parent subIx
               in InternalNode $ go (level - keyBits) children
          | otherwise = newPath (level - keyBits) tail

        -- we subtract one because we want to find where the tail goes
        -- we don't care about the least significant bits as we are only inserting into the parent
        -- this means that subtracting one will do
        subIx = ((size - 1) .<<. level) .&. keyMask

newPath :: Int -> Array a -> Node a
newPath 0 tail = DataNode tail
newPath level tail = InternalNode $ Array.singleton $! newPath (level - keyBits) tail

unsafeIndex :: Vector a -> Int -> a
unsafeIndex vec ix
  | ix >= tailOffset vec = Array.index (tail vec) (ix .&. keyMask)
  | otherwise = go (shift vec - keyBits) (Array.index (init vec) (ix .>>. shift vec))
  where
    go 0 (DataNode as) = Array.index as (ix .&. keyMask)
    go 0 (InternalNode _) = impossibleError
    go level (InternalNode ns) = go (level - keyBits) (Array.index ns ix')
      where
        ix' = (ix .>>. level) .&. keyMask
    go _level (DataNode _) = impossibleError

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

update :: Vector a -> Int -> a -> Vector a
update vec@RootNode {size, shift, tail} ix a
  -- Invalid index. This funny business uses a single test to determine whether
  -- ix is too small (negative) or too large (at least sz).
  | (fromIntegral ix :: Word) >= fromIntegral size = vec
  | ix >= tailOffset vec = vec {tail = Array.update tail (ix .&. keyMask) a}
  | otherwise = vec {init = go shift (init vec)}
  where
    go level vec
      | level == 5 =
          let !node = DataNode $ Array.update (getDataNode vec') (ix .&. keyMask) a
           in Array.update vec ix' node
      | otherwise =
          let !node = go (level - keyBits) (getInternalNode vec')
           in Array.update vec ix' (InternalNode node)
      where
        ix' = (ix .>>. level) .&. keyBits
        vec' = Array.index vec ix'

-- | The index of the first element of the tail of the vector (that is, the
-- *last* element of the list representing the tail). This is also the number
-- of elements stored in the array tree.
--
-- Caution: Only gives a sensible result if the vector is nonempty.
tailOffset :: Vector a -> Int
tailOffset v = (length v - 1) .&. ((-1) .>>. keyBits)
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
