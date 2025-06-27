{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Packed.Unpackable (
    Unpackable (..),
    PackedReader,
    unpack,
    unpack',
    runReader,
    readerWithoutShift,
) where

import Data.Packed.Internal
import Data.Packed.Packed
import Data.Packed.Reader
import Foreign (Storable (peek, sizeOf), castPtr, plusPtr)

-- | An 'Unpackable' is a value that can be read (i.e. deserialised) from a 'Data.Packed' value
class Unpackable a where
    -- | The 'PackedReader' to unpack a value of that type
    reader :: PackedReader '[a] r a

instance (Storable a) => Unpackable a where
    {-# INLINE reader #-}
    reader = mkPackedReader $ \ptr l ->
        let !n = unsafeDupablePerformIO $ Foreign.peek (castPtr ptr)
            !shiftedCount = sizeOf n
            !l1 = l - shiftedCount
            !ptr1 = ptr `plusPtr` shiftedCount
         in (# n, ptr1, l1 #)

{-# INLINE readerWithoutShift #-}

-- | In a `PackedReader`, reads a value without moving the cursor
readerWithoutShift :: (Unpackable a) => PackedReader (a ': r) (a ': r) a
readerWithoutShift = mkPackedReader $ \ptr len ->
    let
        !(# !a, _, _ #) = runPackedReader reader ptr len
     in
        (# a, ptr, len #)

{-# INLINE unpack #-}

-- | Deserialise a value from a 'Data.Packed.Packed'.
--
-- Returns the unconsumed 'Data.Packed.Packed' portion
unpack :: (Unpackable a) => Packed (a ': r) -> (a, Packed r)
unpack = runReader reader

{-# INLINE unpack' #-}

-- | Same as 'unpack', but throws away the unconsumed bytes
unpack' :: (Unpackable a) => Packed (a : r) -> a
unpack' p = fst $ unpack p
