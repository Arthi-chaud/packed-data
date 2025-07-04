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

-- | Typeclass for values that can be packed, i.e. written inside a 'Data.Packed.Needs.Needs'
class Packable a where
    write :: a -> NeedsWriter a r t

instance (Storable a) => Packable a where
    write = writeStorable

-- | Shortcut to produce a 'Data.Packed.Packed' buffer from a single 'Packable' value
pack :: (Packable a) => a -> Packed '[a]
pack = runBuilder . write
