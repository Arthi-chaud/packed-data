{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Packed.FieldSize (
    FieldSize (..),
    skipWithFieldSize,
    isolate,
    getFieldSizeFromPacked,
    writeWithFieldSize,
    readerWithFieldSize,
    applyNeedsWithFieldSize,
) where

import qualified Control.Functor.Linear as L
import qualified Data.ByteString as BS
import Data.Packed.Internal
import Data.Packed.Needs
import Data.Packed.Packable
import Data.Packed.Packed
import Data.Packed.Reader hiding (return)
import qualified Data.Packed.Reader as R
import Data.Packed.Skippable (Skippable (..), unsafeSkipN)
import Data.Packed.Unpackable
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import GHC.Int
import qualified System.IO.Linear as L
import Unsafe.Linear
import Prelude hiding (read)

-- | Type representation for the size of a packed data.
-- The size is in bytes.
--
-- __Note__: Take a look at the 'Data.Packed.TH.PackingFlag's to understand how to use it
newtype FieldSize = FieldSize Int32 deriving (Num, Enum, Real, Ord, Eq)

deriving instance Integral FieldSize

instance {-# OVERLAPPING #-} Packable FieldSize where
    write (FieldSize value) needs = write value (unsafeCastNeeds needs)

instance {-# OVERLAPPING #-} Unpackable FieldSize where
    reader = mkPackedReader $ \packed l ->
        let
            !(# fieldSize, rest, l1 #) = runPackedReader reader packed l
         in
            (# FieldSize fieldSize, rest, l1 #)

instance {-# OVERLAPPING #-} Skippable FieldSize where
    skip = unsafeSkipN (sizeOf (1 :: Int32))

{-# INLINE getFieldSizeFromPacked #-}

-- | Returns the size of the packed value.
--
-- __Warning:__ For this to be accurate, there should only be one value packed in the binary strea.
getFieldSizeFromPacked :: Packed '[a] -> FieldSize
getFieldSizeFromPacked packed = FieldSize (fromIntegral $ BS.length (fromPacked packed))

{-# INLINE skipWithFieldSize #-}

-- | Allows skipping over a field without having to unpack it
skipWithFieldSize :: PackedReader '[FieldSize, a] r ()
skipWithFieldSize = mkPackedReader $ \packed l ->
    let
        !(# FieldSize s, packed1, l1 #) = runPackedReader reader packed l
        !size64 = fromIntegral s
     in
        (# (), packed1 `plusPtr` size64, l1 - size64 #)

{-# INLINE writeWithFieldSize #-}

-- | Write a value into a 'Data.Packed.Needs.Needs', along with its 'FieldSize'
--
-- Note: Universal quantifier is nedded for GHC < 9.10, because of ScopedTypeVariables
writeWithFieldSize :: forall a r t. (Packable a) => a -> NeedsWriter' '[FieldSize, a] r t
writeWithFieldSize a = withFieldSize (write a)

{-# INLINE withFieldSize #-}
withFieldSize :: NeedsBuilder (a ': r) t r t -> NeedsBuilder (FieldSize ': a ': r) t r t
withFieldSize cont needs = L.do
    let !indirectionSize = sizeOf (0 :: Int32)
    -- Reallocating the buffer so that the fieldsize can fit
    !newNeeds <- guardRealloc indirectionSize needs
    -- Get the position of the buffer where the FS will be
    let !(# fieldSizeOffset, newNeeds1 #) = getOffset newNeeds
    -- Shift the cursor
    !writtenNeeds <- cont (unsafeShiftNeedsPtr indirectionSize newNeeds1)
    -- Get the final position of the cursor
    let !(# finalCursor, writtenNeeds1 #) = getOffset writtenNeeds
        !(# og, writtenNeeds2 #) = getOrigin writtenNeeds1
    () <-
        toLinear3
            ( \finalCursor' fsPosition og' fsSize ->
                let
                    -- Count the number of bytes that were written
                    !writtenBytes = intToInt32# (finalCursor' -# (fsPosition +# unInt fsSize))
                 in
                    -- And write it

                    L.fromSystemIO (poke (Ptr $ og' `plusAddr#` fsPosition) (I32# writtenBytes))
            )
            finalCursor
            fieldSizeOffset
            og
            indirectionSize
    L.return writtenNeeds2

{-# INLINE applyNeedsWithFieldSize #-}
applyNeedsWithFieldSize :: Needs '[] '[a] -> NeedsWriter' (FieldSize ': a ': '[]) r t
applyNeedsWithFieldSize n = withFieldSize (applyNeeds n)

{-# INLINE readerWithFieldSize #-}

-- | Produces a reader for a value preceded by its 'FieldSize'
readerWithFieldSize :: (Unpackable a) => PackedReader '[FieldSize, a] r a
readerWithFieldSize = skip R.>> reader

{-# INLINE isolate #-}

-- | Splits the 'Packed' value, and isolate the first encoded value.
isolate :: PackedReader '[FieldSize, a] r (Packed '[a])
isolate = mkPackedReader $ \packed l ->
    let
        !(# FieldSize s, packed1, l1 #) = runPackedReader reader packed l
        !sizeInt = fromIntegral s
     in
        (# unsafeToPacked' packed1 sizeInt, packed1 `plusPtr` sizeInt, l1 - sizeInt #)
