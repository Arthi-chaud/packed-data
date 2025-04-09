{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Packed.Case (PackedCase (..)) where

import Data.Kind (Type)

-- | Typeclass for 'case' functions on packed data
class PackedCase a where
    type CaseSig a :: Type
    case_ :: CaseSig a
