{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Build (benchmark) where

import Control.DeepSeq
import Criterion.Main
import Data.Packed
import Data.Packed.Needs
import qualified Data.Packed.Needs as N
import GHC.Generics (Generic, Generic1)
import Utils
import Prelude hiding (sum)

data Tree1 a = Leaf1 !a | Node1 !(Tree1 a) !(Tree1 a) deriving (Generic, Generic1)

$(mkPacked ''Tree1 [])

instance (NFData a) => NFData (Tree1 a)

benchmark :: [Int] -> Benchmark
benchmark depths =
    bgroup
        "build"
        $ fmap buildTreeWithDepth depths

buildTreeWithDepth :: Int -> Benchmark
buildTreeWithDepth n =
    bgroup
        (depthGroupName n)
        [ bench nativeTestName $ nf buildNativeTree n
        , bench packedTestName $ nf (runBuilder . buildPackedTree) n
        ]

buildNativeTree :: Int -> Tree1 Int
buildNativeTree 0 = Leaf1 1
buildNativeTree n = Node1 subTree subTree
  where
    !subTree = buildNativeTree (n - 1)

buildPackedTree :: Int -> NeedsBuilder (Tree1 Int ': r) '[Tree1 Int] r '[Tree1 Int]
buildPackedTree 0 = writeConLeaf1 (1 :: Int)
buildPackedTree n = \needs -> startNode1 needs N.>>= subTree N.>>= subTree
  where
    subTree :: NeedsBuilder (Tree1 Int ': r1) '[Tree1 Int] r1 '[Tree1 Int]
    !subTree = buildPackedTree (n - 1)
