{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Packed.Packable (Packable (..), pack) where

import ByteString.StrictBuilder
import Data.Packed.Needs (
    Needs (..),
    NeedsWriter,
    finish,
    mkNeedsBuilder,
    withEmptyNeeds,
 )
import Data.Packed.Packed (Packed)
import Foreign
import Prelude hiding (length)

class Packable a where
    write :: a -> NeedsWriter a r t

instance (Storable a) => Packable a where
    write v = mkNeedsBuilder (\(Needs b) -> Needs (b <> storable v))

pack :: (Packable a) => a -> Packed '[a]
pack a = finish (withEmptyNeeds (write a))
