{-# OPTIONS_GHC -Wno-orphans #-}

module Pack (benchmark) where

import Control.DeepSeq
import Criterion.Main
import Data.Packed
import Utils
import Prelude hiding (sum)

data Tree1 a = Leaf1 a | Node1 !(Tree1 a) !(Tree1 a)
data Tree2 a = Leaf2 a | Node2 !(Tree2 a) !(Tree2 a)

$(mkPacked ''Tree1 [])
$(mkPacked ''Tree2 [])

instance NFData (Tree1 a) where
    rnf (Leaf1 a) = a `seq` ()
    rnf (Node1 l r) = l `seq` r `seq` ()

benchmark :: [Int] -> Benchmark
benchmark depths =
    bgroup
        "pack"
        $ fmap buildTreeWithDepth depths

buildTreeWithDepth :: Int -> Benchmark
buildTreeWithDepth n =
    bgroup
        (depthGroupName n)
        [ bench packedTestName $ nf Data.Packed.pack (buildNativeTree n)
        , bench packedWithFieldSizeTestName $ nf Data.Packed.pack (buildNativeTree2 n)
        ]

buildNativeTree :: Int -> Tree1 Int
buildNativeTree 0 = Leaf1 1
buildNativeTree n = Node1 subTree subTree
  where
    subTree = buildNativeTree (n - 1)

buildNativeTree2 :: Int -> Tree2 Int
buildNativeTree2 0 = Leaf2 1
buildNativeTree2 n = Node2 subTree subTree
  where
    subTree = buildNativeTree2 (n - 1)
