{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Packed.Internal where

import GHC.Base
import GHC.Exts

{-# INLINE unPtr #-}
unPtr :: Ptr a %1 -> Addr#
unPtr (Ptr a) = a

{-# INLINE unInt #-}
unInt :: Int %1 -> Int#
unInt (I# i) = i
