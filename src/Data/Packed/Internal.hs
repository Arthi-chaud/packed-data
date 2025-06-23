{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Packed.Internal where

import GHC.Base

{-# INLINE unsafeRunIO #-}
unsafeRunIO :: IO a -> a
unsafeRunIO (IO a) = case runRW# a of
    (# _, !r #) -> r
