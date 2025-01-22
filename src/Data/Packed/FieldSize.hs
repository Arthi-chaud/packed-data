{-# LANGUAGE ScopedTypeVariables #-}

module Data.Packed.FieldSize (
    FieldSize (..),
    skipWithFieldSize,
    isolate,
    getFieldSizeFromPacked,
    writeWithFieldSize,
    readerWithFieldSize,
    applyNeedsWithFieldSize,
) where

import ByteString.StrictBuilder (builderLength)
import Control.Monad
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Packed.Needs
import qualified Data.Packed.Needs as N
import Data.Packed.Packable
import Data.Packed.Packed
import Data.Packed.Reader hiding (return)
import qualified Data.Packed.Reader as R
import Data.Packed.Skippable (Skippable (..), unsafeSkipN)
import Data.Packed.Unpackable
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (read)

-- | Type representation for the size of a packed data.
-- The size is in bytes.
--
-- __Note__: Take a look at the 'Data.Packed.TH.PackingFlag's to understand how to use it
newtype FieldSize = FieldSize Int32

instance {-# OVERLAPPING #-} Packable FieldSize where
    write (FieldSize value) = mkNeedsBuilder unsafeCastNeeds N.>> write value

instance {-# OVERLAPPING #-} Unpackable FieldSize where
    reader = mkPackedReader $ \packed l -> do
        (fieldSize, rest, l1) <- runPackedReader reader packed l
        return (FieldSize fieldSize, rest, l1)

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
skipWithFieldSize = mkPackedReader $ \packed l -> do
    (FieldSize s, packed1, l1) <- runPackedReader reader packed l
    let size64 = fromIntegral s
    return ((), packed1 `plusPtr` size64, l1 - size64)

{-# INLINE writeWithFieldSize #-}

-- | Write a value into a 'Data.Packed.Needs.Needs', along with its 'FieldSize'
--
-- Note: Universal quantifier is nedded for GHC < 9.10, because of ScopedTypeVariables
writeWithFieldSize :: forall a r t. (Packable a) => a -> NeedsWriter' '[FieldSize, a] r t
writeWithFieldSize a = write (FieldSize size) N.>> applyNeeds aNeeds
  where
    size = fromIntegral (builderLength aBuilder)
    aNeeds :: Needs '[] '[a]
    aNeeds@(Needs aBuilder) = withEmptyNeeds (write a)

{-# INLINE applyNeedsWithFieldSize #-}
applyNeedsWithFieldSize :: Needs '[] '[a] -> NeedsWriter' (FieldSize ': a ': '[]) r t
applyNeedsWithFieldSize n@(Needs builder) = write (FieldSize (fromIntegral (builderLength builder))) N.>> applyNeeds n

{-# INLINE readerWithFieldSize #-}

-- | Produces a reader for a value preceded by its 'FieldSize'
readerWithFieldSize :: (Unpackable a) => PackedReader '[FieldSize, a] r a
readerWithFieldSize = skip R.>> reader

{-# INLINE isolate #-}

-- | Splits the 'Packed' value, and isolate the first encoded value.
isolate :: PackedReader '[FieldSize, a] r (Packed '[a])
isolate = mkPackedReader $ \packed l -> do
    (FieldSize s, packed1, l1) <- runPackedReader reader packed l
    let sizeInt = fromIntegral s
    return (unsafeToPacked' packed1 sizeInt, packed1 `plusPtr` sizeInt, l1 - sizeInt)
