{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Packed.Case (PackedCase (..)) where

import Data.Kind (Type)

-- | Typeclass for 'case' functions on packed data
class PackedCase (a :: Type) (r :: [Type]) (b :: Type) where
    type CaseSig a r b :: Type
    case_ :: CaseSig a r b
