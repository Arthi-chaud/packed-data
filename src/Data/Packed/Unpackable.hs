{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
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

import Control.Monad.Identity
import Data.Packed.Packed
import Data.Packed.Reader hiding (return)
import Foreign (Storable (peek, sizeOf), castPtr, plusPtr)
import GHC.Exts
import GHC.IO

-- | An 'Unpackable' is a value that can be read (i.e. deserialised) from a 'Data.Packed' value
class Unpackable a where
    -- | The 'Data.Packed.Reader.PackedReader' to unpack a value of that type
    reader :: PackedReader '[a] r a

instance (Storable a) => Unpackable a where
    {-# INLINE reader #-}
    reader = PackedReader $ \(PF ptr int) -> case runRW# (unIO (peek $ castPtr ptr)) of
        (# _, !n #) ->
            let
                !sizeOfN = sizeOf n
                !shiftedPtr = ptr `plusPtr` sizeOfN
                !shiftedSize = int - sizeOfN
             in
                return (n, PF shiftedPtr shiftedSize)

{-# INLINE readerWithoutShift #-}

-- | In a `PackedReader`, reads a value without moving the cursor
readerWithoutShift :: (Unpackable a) => PackedReader '[] (a ': r) a
readerWithoutShift = mkPackedReader $ \pf ->
    let
        Identity !(!a, !_) = runReaderStep reader pf
     in
        return (a, pf)

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
