{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Packed.Packable (Packable (..), pack) where

import Data.Packed.Needs (
    NeedsWriter,
    runBuilder,
    writeStorable,
 )
import Data.Packed.Packed (Packed)
import Foreign
import Prelude hiding (length)

class Packable a where
    write :: a -> NeedsWriter a r t

instance (Storable a) => Packable a where
    write = writeStorable

pack :: (Packable a) => a -> Packed '[a]
pack = runBuilder . write
