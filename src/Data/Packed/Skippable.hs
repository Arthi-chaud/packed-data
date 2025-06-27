{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Packed.Skippable (Skippable (..), unsafeSkipN) where

import Data.Kind
import Data.Packed.Reader
import Foreign (plusPtr)
import Foreign.Storable

class Skippable a where
    -- A function that moves the cursor at the end of the first packed value in the buffer.
    --
    -- Beware, this does not rely on `Data.Packed.FieldSize`, therefore it usually entails a traversals
    skip :: PackedReader '[a] r ()

instance (Storable a) => Skippable a where
    skip = unsafeSkipN (sizeOf (undefined :: a))

{-# INLINE unsafeSkipN #-}

-- | UNSAFE: Shifts the cursor to n bytes to the right.
unsafeSkipN :: forall (a :: [Type]) (r :: [Type]). Int -> PackedReader a r ()
unsafeSkipN n = mkPackedReader $ \ptr l -> (# (), ptr `plusPtr` n, l - n #)
