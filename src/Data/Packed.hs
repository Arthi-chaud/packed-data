-- |
--
-- Packed data is data serialised into a binary format that is usable as-is, meaning there is no need to parse it to be able to use it. Another perk of such format is that it can be stored in files easily.
--
-- `packed-data` allows using packed data type-safely, without explicit pointer arithmetic.
--
-- Check out examples here: https://github.com/Arthi-chaud/packed-data/tree/main/examples
--
-- Example of a tree traversal:
--
-- @
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TemplateHaskellQuotes #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE QualifiedDo #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE UnboxedTuples #-}
--
-- import Data.Packed
-- import Data.Packed.Reader
-- import qualified Data.Packed.Reader as R
--
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- \$('Data.Packed.mkPacked' ''Tree [])
--
-- myTree :: Tree Int
-- myTree = Node (Leaf 1) (Leaf 2)
--
-- packedTree :: 'Data.Packed.Packed' '[Tree Int]
-- packedTree = 'pack' myTree
--
-- -- The two following functions do th same thing
--
-- sumPacked1 :: 'Data.Packed.Reader.PackedReader' '[Tree Int] r Int
-- sumPacked1 =
--     caseTree -- Generated
--         ( R.do
--             !n <- 'reader'
--             R.'Data.Packed.Reader.return' n
--         )
--         ( R.do
--             !left <- sumPacked1
--             !right <- sumPacked1
--             let !res = left + right
--             R.'Data.Packed.Reader.return' res
--         )
--
-- -- The patterns 'PackedLead' and 'PackedNode' are generated
-- sumPacked2 :: 'Data.Packed.Reader.PackedReader' '[Tree Int] r Int
-- sumPacked2 = 'Data.Packed.Reader.mkPackedReader' $ \case
--     PackedLeaf l -> 'reader' `Data.Packed.Reader.with` l
--     PackedNode n -> 'Data.Packed.Reader.threadedWith' n $ R.do
--         !left <- sumPacked2
--         !right <- sumPacked2
--         let !res = left + right
--         R.'return' res
--
-- runSum :: Int
-- runSum = fst $ 'Data.Packed.Reader.runReader' sumPacked1 packedTree
-- @
module Data.Packed (
    -- * Classes
    Packable (..),
    pack,
    Unpackable (..),
    readerWithoutShift,
    unpack,
    unpack',

    -- * Needs
    Needs,
    runBuilder,
    writeWithFieldSize,
    unsafeCastNeeds,

    -- * Packed
    Packed,
    skipWithFieldSize,
    isolate,
    fromPacked,
    unsafeToPacked,
    unsafeCastPacked,

    -- * PackedReader
    PackedReader,
    mkPackedReader,
    runReader,
    readerWithFieldSize,

    -- * Code generation
    mkPacked,
    PackingFlag (..),

    -- * Utils
    FieldSize (..),
    getFieldSizeFromPacked,
    Skippable (..),
) where

import Data.Packed.FieldSize
import Data.Packed.Needs
import Data.Packed.Packable
import Data.Packed.Packed
import Data.Packed.Reader
import Data.Packed.Skippable
import Data.Packed.TH
import Data.Packed.Unpackable
