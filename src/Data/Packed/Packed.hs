{-# LANGUAGE CPP #-}
module Data.Packed.Packed (Packed (..), unsafeToPacked, unsafeToPacked', fromPacked, unsafeCastPacked, duplicate) where

import Control.DeepSeq
import Data.ByteString (copy)
import Data.ByteString.Internal
import Data.Kind (Type)
import Foreign (Ptr)
import GHC.Exts (Ptr (Ptr))
import GHC.ForeignPtr (ForeignPtr (ForeignPtr), ForeignPtrContents (FinalPtr))

-- | A buffer that contains one or more packed (i.e. serialised) values.
-- The order of the values in the buffer is defined by the 'l' type list
newtype Packed (l :: [Type]) = Packed ByteString

instance NFData (Packed a) where
    rnf packed = fromPacked packed `Prelude.seq` ()

-- | Duplicates a 'Packed' buffer. The returned 'Packed' is independent from the source one.
duplicate :: Packed a -> Packed a
duplicate (Packed bs) = Packed $ copy bs

{-# INLINE unsafeToPacked #-}

-- | UNSAFE: Casts a generic 'ByteString' into a 'Data.Packed.Needs'
unsafeToPacked :: ByteString -> Packed a
unsafeToPacked = Packed

{-# INLINE fromPacked #-}

-- | Extracts the raw buffer from a 'Data.Packed' value
fromPacked :: Packed a -> ByteString
fromPacked (Packed bs) = bs

{-# INLINE unsafeCastPacked #-}

-- | UNSAFE: Casts a typed 'Packed' value into another 'Packed' value of another type
unsafeCastPacked :: Packed a -> Packed b
unsafeCastPacked = unsafeToPacked . fromPacked

unsafeToPacked' :: Ptr a -> Int -> Packed b
unsafeToPacked' (Ptr addr) l = 
#if MIN_VERSION_bytestring(0,11,0)
    Packed (BS (ForeignPtr addr FinalPtr) l)
#else
    Packed (PS (ForeignPtr addr FinalPtr) 0 l)
#endif
