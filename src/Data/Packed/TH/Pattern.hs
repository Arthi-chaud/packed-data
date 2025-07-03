module Data.Packed.TH.Pattern (genPatterns, tagReaderFName) where

import Data.Packed.Reader (PackedFragment)
import Data.Packed.TH.Flag (PackingFlag (..))
import Data.Packed.TH.Utils
import Language.Haskell.TH

-- | Generates patterns for packed values of the given type
--
--  __Example:__
--
-- For the 'Tree' data type, it generates the following patterns:
--
-- @
-- PackedLeaf :: (PackedFragment (a ': r)) -> PackedFragment (Tree a ': r)
--
-- PackedNode :: (PackedFragment (Tree A ': Tree a ': r)) -> PackedFragment (Tree a ': r)
-- @
--
-- More specifically, it generates the following code:
--
-- @
--
-- {-# INLINE readTreeTag #-}
-- readTreeTag :: PackedFragment (Tree a ': r) -> (# Word8#, PackedFragment n #)
-- readTreeTag (PF ptr@(Ptr addr) i) = case readWord8OffAddr# addr 0# realWorld# of
--     (# _, t #) -> case W8# t of
--         0 -> (# t, PF (ptr `plusPtr` 1) (i - 1) #)
--         1 -> (# t, PF (ptr `plusPtr` 1) (i - 1) #)
--         _ -> error $ "Bag tag: Got " ++ show (W8# t)
--
-- {-# INLINE PackedLeaf #-}
-- pattern PackedLeaf :: PackedFragment (a ': r) -> PackedFragment (Tree a ': r)
-- pattern PackedLeaf pf <- (readTreeTag -> (# 0#Word8 :: Word8#, pf #))
--
-- {-# INLINE PackedNode #-}
-- pattern PackedNode :: PackedFragment (Tree a ': Tree a ': r) -> PackedFragment (Tree a ': r)
-- pattern PackedNode pf <- (readTreeTag -> (# 1#Word8 :: Word8#, pf #))
--
-- {-# COMPLETE PackedLeaf, PackedNode #-}
-- @
genPatterns :: [PackingFlag] -> Name -> Q [Dec]
genPatterns flags tyName = return []

tagReaderFName :: Name -> Name
tagReaderFName tyName = mkName $ "read" ++ sanitizeConName tyName ++ "Tag"
